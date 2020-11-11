package text

import java.io.{PrintWriter, StringWriter}

import scala.io.Source
import scala.util.Using

import scala.util.parsing.combinator.{RegexParsers, PackratParsers}

object TeXPEGs extends RegexParsers with PackratParsers {
	def parseTeX(str: String): DocTeX = parseAll(doc, str) match {
		case Success(ast, _) => ast
		case fail: NoSuccess => sys.error(fail.msg)
	}
	override def skipWhitespace = false
	lazy val id = """[@A-Za-z]+\*?""".r
	lazy val yen: Parser[YenTeX] = "\\" ~> id ^^ YenTeX
	lazy val com: Parser[TeX] = "%" ~ ".*".r ^^ (_ => StrTeX(""))
	lazy val str: Parser[TeX] = """[^\\\{\}\[\]%\$]+""".r ^^ StrTeX
	lazy val bra: Parser[TeX] = "{" ~> doc <~ "}" ^^ ArgTeX
	lazy val sqb: Parser[TeX] = "[" ~> doc <~ "]" ^^ OptTeX
	lazy val esc: Parser[TeX] = "\\" ~> """[\\{}_&%\$#\|;!, ]""".r ^^ EscTeX
	lazy val env: Parser[TeX] = ("\\begin{" ~> id <~ "}") ~ arg ~ doc <~ ("\\end{" ~> id <~ "}") ^^ {
		case name~args~body => EnvAppTeX(name,args,body)
	}
	lazy val cmd: Parser[TeX] = not("\\end{") ~> yen ~ arg ^^ {case name~args => CmdAppTeX(name,args)}
	lazy val arg: Parser[DocTeX] = (yen.+ ~ (sqb | bra).*) ^^ {case y~args => DocTeX(y ++ args)} | ((sqb | bra).* ^^ DocTeX)
	lazy val mat: Parser[TeX] = "$" ~> (esc | cmd | bra | sqb | str).* <~ "$" ^^ DocTeX ^^ MatTeX
	lazy val doc: Parser[DocTeX] = (com | esc | lst | env | vrb | cmd | bra | sqb | str | mat).* ^^ DocTeX
	lazy val vrb: Parser[TeX] = (("\\verb#" ~> "[^#]*".r <~ "#") | ("\\verb|" ~> """[^\|]*""".r <~ "|")) ^^ VrbTeX
	lazy val lst: Parser[TeX] = "\\begin{Verbatim}" ~> arg ~ (not("\\end") ~> "[\\S\\s]".r).* <~ "\\end{Verbatim}" ^^ {
		case args~lst => LstTeX(args.body.head, lst.mkString)
	}
}

case class Scope(cmds: Seq[CmdTeX], envs: Seq[EnvTeX], out: Option[Scope]) {
	val cmdsTable = cmds.map(tex => tex.name -> tex).to(collection.mutable.Map)
	val envsTable = envs.map(tex => tex.name -> tex).to(collection.mutable.Map)
	def install(cmd: CmdTeX) = cmdsTable.getOrElseUpdate(cmd.name, cmd)
	def install(env: EnvTeX) = envsTable.getOrElseUpdate(env.name, env)
	def cmd(name: YenTeX): Option[CmdTeX] = cmdsTable.get(name).orElse(out.map(_.cmd(name)).flatten)
	def env(name: String): Option[EnvTeX] = envsTable.get(name).orElse(out.map(_.env(name)).flatten)
}

object Root extends Scope(Seq(
	LabelCmdTeX,
	RefCmdTeX,
	EqRefCmdTeX,
	TabRefCmdTeX,
	SubRefCmdTeX,
	SubFigRefCmdTeX,
	CaptionCmdTeX,
	IncludeGraphicsCmdTeX,
	SubFloatCmdTeX,
	ChapterCmdTeX,
	SectionCmdTeX,
	SubSectionCmdTeX,
	TextBfCmdTeX,
	TextItCmdTeX,
	TextTtCmdTeX,
	HFillCmdTeX,
	TopRuleCmdTeX,
	MidRuleCmdTeX,
	BottomRuleCmdTeX,
	BmCmdTeX,
	ColonEqqCmdTeX,
	MathChoiceCmdTeX,
	CenteringCmdTeX,
	QuadCmdTeX,
	DefCmdTeX,
	GDefCmdTeX,
	LetCmdTeX,
	LetLtxMacroCmdTeX,
	DocumentClassCmdTeX,
	MakeTitleCmdTeX,
	TableOfContentsCmdTeX,
	UsePackageCmdTeX,
	RequirePackageCmdTeX,
	NewCommandCmdTeX,
	RenewCommandCmdTeX,
	NewDocumentCommandCmdTeX,
	RenewDocumentCommandCmdTeX,
	NewDocumentEnvironmentCmdTeX,
	RenewDocumentEnvironmentCmdTeX,
	DeclareMathOperatorCmdTeX
), Seq(
	DocumentEnvTeX,
	EquationEnvTeX,
	FigureEnvTeX,
	TabularEnvTeX
), None)

trait TeX {
	def cvt(scope: TeX): TeX
	def str(scope: TeX): String
	def peel(scope: TeX) = str(scope) match {
		case str if str.startsWith("{") && str.endsWith("}") => str.tail.init
		case str if str.startsWith("[") && str.endsWith("]") => str.tail.init
		case str => str
	}
	def lab(prefix: String): Seq[String]
	def cap: Seq[String]
	def isMath: Boolean = false
}

abstract class LeafTeX extends TeX {
	override def cvt(scope: TeX): TeX = this
	override def lab(prefix: String): Seq[String] = Seq()
	override def cap: Seq[String] = Seq()
}

abstract class TreeTeX(children: TeX*) extends TeX {
	def lab(prefix: String) = children.map(_.lab(prefix)).flatten
	def cap = children.map(_.cap).flatten
}

abstract class CmdTeX(raw: String) {
	def name = YenTeX(raw.replaceAll("""\\""", ""))
	def apply(app: CmdAppTeX, scope: TeX): CmdBodyTeX
}

class CmdBodyTeX(app: CmdAppTeX, cmd: CmdTeX) extends TreeTeX(app.args) {
	override def cvt(scope: TeX): TeX = cmd(CmdAppTeX(cmd.name, app.args.cvt(scope)), scope)
	override def str(scope: TeX) = ""
}

abstract class EnvTeX(val name: String) {
	def apply(app: EnvAppTeX, scope: TeX): EnvBodyTeX
}

class EnvBodyTeX(app: EnvAppTeX, env: EnvTeX) extends TreeTeX(app) {
	override def cvt(scope: TeX): TeX = env(EnvAppTeX(env.name, app.args.cvt(this), app.body.cvt(scope)), this)
	override def str(scope: TeX) = app.body.str(scope)
}

case class CmdAppTeX(name: YenTeX, args: DocTeX) extends TreeTeX(args) {
	override def cvt(scope: TeX) = Root.cmd(name).map(_(this, scope).cvt(scope)).getOrElse(CmdAppTeX(name, args.cvt(scope)))
	override def str(scope: TeX) = name.str(scope).concat(args.str(scope))
}

case class EnvAppTeX(name: String, args: DocTeX, body: TeX) extends TreeTeX(args, body) {
	override def cvt(scope: TeX) = Root.env(name).map(_(this, this).cvt(this)).getOrElse(EnvAppTeX(name, args.cvt(this), body.cvt(this)))
	override def str(scope: TeX) = scope.isMath match {
		case true => """\begin{%1$s}%2$s%3$s\end{%1$s}""".format(name, args.str(scope), body.str(scope))
		case false => body.str(this)
	}
}

case class YenTeX(text: String) extends LeafTeX {
	override def cvt(scope: TeX) = CmdAppTeX(this, DocTeX(Seq())).cvt(scope)
	override def str(scope: TeX) = """\""".concat(text)
}

case class OptTeX(body: TeX) extends TreeTeX(body) {
	override def cvt(scope: TeX) = OptTeX(body.cvt(scope))
	override def str(scope: TeX) = """[%s]""".format(body.str(scope))
}

case class ArgTeX(body: TeX) extends TreeTeX(body) {
	override def cvt(scope: TeX) = ArgTeX(body.cvt(scope))
	override def str(scope: TeX) = """{%s}""".format(body.str(scope))
}

case class StrTeX(text: String) extends LeafTeX {
	override def str(scope: TeX) = text.replace('~', ' ')
}

case class EscTeX(char: String) extends LeafTeX {
	override def str(scope: TeX) = "\\".concat(char)
}

case class VrbTeX(body: String) extends LeafTeX {
	override def str(scope: TeX) = s"`${body}`"
}

case class LstTeX(lang: TeX, body: String) extends LeafTeX {
	override def str(scope: TeX) = s"```${body}```"
}

case class MatTeX(body: TeX) extends TreeTeX(body) {
	override def cvt(scope: TeX) = MatTeX(body.cvt(this))
	override def str(scope: TeX) = s"$$${body.str(this).trim}$$"
	override def isMath: Boolean = true
}

case class DocTeX(body: Seq[TeX]) extends TreeTeX(body:_*) {
	override def cvt(scope: TeX) = DocTeX(body.map(_.cvt(scope)))
	override def str(scope: TeX) = body.map(_.str(scope)).mkString
	override def peel(scope: TeX) = body.map(_.peel(scope)).mkString
}

object StandardLabelFormat {
	def apply(label: String) = label.split(":", 2).toSeq match {
		case Seq("chap", lab) => "sec:".concat(lab)
		case Seq("sect", lab) => "sec:".concat(lab)
		case Seq("tab",  lab) => "tbl:".concat(lab)
		case other => other.mkString(":")
	}
}

object LabelCmdTeX extends CmdTeX("label") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def lab(prefix: String) = Seq(label).filter(_.startsWith(prefix))
	}
}

object RefCmdTeX extends CmdTeX("ref") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def str(scope: TeX) = "[@%s]".format(label)
	}
}

object EqRefCmdTeX extends CmdTeX("eqref") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def str(scope: TeX) = "[@eq:%s]".format(label)
	}
}

object TabRefCmdTeX extends CmdTeX("tabref") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		val label = StandardLabelFormat("tbl:".concat(app.args.body.head.peel(scope)))
		override def str(scope: TeX) = "Table ".concat(scope.lab(label.concat(":")).padTo(1, label).map("[@%s]".format(_)).mkString(", "))
	}
}

object SubRefCmdTeX extends CmdTeX("subref") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def str(scope: TeX) = "[@%s]".format(label)
	}
}

object SubFigRefCmdTeX extends CmdTeX("subfigref") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		val label = StandardLabelFormat(app.args.body.map(_.peel(scope)).mkString(":"))
		override def str(scope: TeX) = "Fig. [@fig:%s]".format(label)
	}
}

object CaptionCmdTeX extends CmdTeX("caption") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def cap = Seq(app.args.body.head.peel(scope))
	}
}

object IncludeGraphicsCmdTeX extends CmdTeX("includegraphics") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		val path = app.args.body.last.peel(scope).replaceAll(".eps$", ".svg")
		override def str(scope: TeX) = {
			val cap = scope.cap.headOption.getOrElse("")
			val lab = scope.lab("fig:").headOption.map("{#%s}".format(_)).getOrElse("")
			"![%s](%s)%s".format(cap, path, lab)
		}
	}
}

object SubFloatCmdTeX extends CmdTeX("subfloat") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def cvt(scope: TeX) = apply(CmdAppTeX(name, app.args.cvt(this)), this)
		override def str(scope: TeX) = app.args.body.last.peel(this)
		override def cap = Seq(app.args.body.head.peel(this))
	}
}

object ChapterCmdTeX extends CmdTeX("chapter") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = {
			val tag = s"# ${app.args.peel(scope)}"
			val lab = this.lab("sec:").headOption.map(" {#%s}".format(_)).getOrElse("")
			tag.concat(lab)
		}
	}
}

object SectionCmdTeX extends CmdTeX("section") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = {
			val tag = s"## ${app.args.peel(scope)}"
			val lab = this.lab("sec:").headOption.map(" {#%s}".format(_)).getOrElse("")
			tag.concat(lab)
		}
	}
}

object SubSectionCmdTeX extends CmdTeX("subsection") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = {
			val tag = s"### ${app.args.peel(scope)}"
			val lab = this.lab("sec:").headOption.map(" {#%s}".format(_)).getOrElse("")
			tag.concat(lab)
		}
	}
}

object TextBfCmdTeX extends CmdTeX("textbf") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = scope.isMath match {
			case true => s"""\\textbf{${app.args.peel(scope)}}"""
			case false => s"**${app.args.peel(scope)}**"
		}
	}
}

object TextItCmdTeX extends CmdTeX("textit") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = scope.isMath match {
			case true => s"""\\textit{${app.args.peel(scope)}}"""
			case false => s"*${app.args.peel(scope)}*"
		}
	}
}

object TextTtCmdTeX extends CmdTeX("texttt") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = scope.isMath match {
			case true => s"""\\texttt{${app.args.peel(scope)}}"""
			case false => app.args.peel(scope)
		}
	}
}

object BmCmdTeX extends CmdTeX("bm") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = s"""\\boldsymbol%s""".format(app.args.str(scope))
	}
}

object ColonEqqCmdTeX extends CmdTeX("coloneqq") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = """:="""
	}
}

object MathChoiceCmdTeX extends CmdTeX("mathchoice") {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this) {
		override def str(scope: TeX) = app.args.body.head.peel(scope)
	}
}

class OutputNothingCmdTeX(name: String) extends CmdTeX(name) {
	def apply(app: CmdAppTeX, scope: TeX) = new CmdBodyTeX(app, this)
}

class OutputNothingEnvTeX(name: String) extends EnvTeX(name) {
	def apply(app: EnvAppTeX, scope: TeX) = new EnvBodyTeX(app, this)
}

object LetLtxMacroCmdTeX extends CmdTeX("LetLtxMacro") {
	def apply(outer: CmdAppTeX, scope: TeX) = new CmdBodyTeX(outer, this) {
		val newName = outer.args.body.head.peel(scope)
		val oldName = outer.args.body.last.peel(scope)
		override def cvt(scope: TeX) = {
			Root.install(new CmdTeX(newName) {
				def apply(inner: CmdAppTeX, scope: TeX) = new CmdBodyTeX(inner, this) {
					val value = inner.args.str(scope)
					override def cvt(scope: TeX) = this
					override def str(scope: TeX) = oldName.concat(value)
				}
			})
			this
		}
	}
}

class BaseNewCommandCmdTeX(name: String) extends CmdTeX(name) {
	def apply(outer: CmdAppTeX, scope: TeX) = new CmdBodyTeX(outer, this) {
		val name = outer.args.body.head.peel(scope)
		val data = outer.args.body.last.peel(scope)
		override def cvt(scope: TeX) = {
			Root.install(new CmdTeX(name) {
				def apply(inner: CmdAppTeX, scope: TeX) = new CmdBodyTeX(inner, this) {
					override def cvt(scope: TeX) = {
						val narg = """#(\d+)""".r.findAllIn(data).distinct.size
						val text = data.replaceAll("""#(\d)""", """%$1\$s""")
						val (args,tail) = inner.args.body.splitAt(narg)
						val full = args ++ Seq.fill(narg - args.size)(StrTeX(""))
						val form = text.format(full.map(_.cvt(scope).peel(scope)):_*)
						val rest = tail.map(_.cvt(scope).peel(scope)).mkString
						TeXPEGs.parseTeX(form.concat(rest)).cvt(scope)
					}
				}
			})
			this
		}
	}
}

object DocumentClassCmdTeX extends OutputNothingCmdTeX("documentclass")
object MakeTitleCmdTeX extends OutputNothingCmdTeX("maketitle")
object TableOfContentsCmdTeX extends OutputNothingCmdTeX("tableofcontents")
object UsePackageCmdTeX extends OutputNothingCmdTeX("usepackage")
object RequirePackageCmdTeX extends OutputNothingCmdTeX("RequirePackage")
object CenteringCmdTeX extends OutputNothingCmdTeX("centering")
object QuadCmdTeX extends OutputNothingCmdTeX("quad")
object HFillCmdTeX extends OutputNothingCmdTeX("hfill")
object TopRuleCmdTeX extends OutputNothingCmdTeX("toprule")
object MidRuleCmdTeX extends OutputNothingCmdTeX("midrule")
object BottomRuleCmdTeX extends OutputNothingCmdTeX("bottomrule")
object DefCmdTeX extends BaseNewCommandCmdTeX("def")
object GDefCmdTeX extends BaseNewCommandCmdTeX("gdef")
object LetCmdTeX extends BaseNewCommandCmdTeX("let")
object NewCommandCmdTeX extends BaseNewCommandCmdTeX("newcommand")
object RenewCommandCmdTeX extends BaseNewCommandCmdTeX("renewcommand")
object NewDocumentCommandCmdTeX extends BaseNewCommandCmdTeX("NewDocumentCommand")
object RenewDocumentCommandCmdTeX extends BaseNewCommandCmdTeX("RenewDocumentCommand")
object DeclareMathOperatorCmdTeX extends BaseNewCommandCmdTeX("DeclareMathOperator*")
object NewDocumentEnvironmentCmdTeX extends OutputNothingCmdTeX("NewDocumentEnvironment")
object RenewDocumentEnvironmentCmdTeX extends OutputNothingCmdTeX("RenewDocumentEnvironment")

object DocumentEnvTeX extends EnvTeX("document") {
	def apply(app: EnvAppTeX, scope: TeX) = new EnvBodyTeX(app, this) {
		override def cvt(scope: TeX) = app.body.cvt(this)
		override def str(scope: TeX) = app.body.str(this)
	}
}

object EquationEnvTeX extends EnvTeX("equation") {
	def apply(app: EnvAppTeX, scope: TeX) = new EnvBodyTeX(app, this) {
		override def str(scope: TeX) = {
			val exp = app.body.str(this).trim
			val lab = this.lab("eq:").headOption.map(" {#%s}".format(_)).getOrElse("")
			"""$$%s$$%s""".format(exp, lab)
		}
		override def isMath: Boolean = true
	}
}

object FigureEnvTeX extends EnvTeX("figure") {
	def apply(app: EnvAppTeX, scope: TeX) = new EnvBodyTeX(app, this) {
		override def str(scope: TeX) = if(cap.size > 1) s"""<div id="${lab("fig:").last}">
${app.body.str(this).trim}

${cap.last}
</div>""" else app.body.str(this)
	}
}

object TabularEnvTeX extends EnvTeX("tabular") {
	def apply(app: EnvAppTeX, scope: TeX) = new EnvBodyTeX(app, this) {
		override def str(scope: TeX) = {
			val ncol = app.args.body.head.str(scope).filterNot(_ == ' ').size
			val rows = app.body.str(scope).replaceAll("&", "|").replaceAll("""\\""", "").trim.linesIterator.toSeq
			val head = Seq(rows.head, Seq.fill(ncol)("---").mkString("|", "|", "|"))
			val body = rows.tail.map("|%s|".format(_))
			val cap = scope.cap.headOption.getOrElse("")
			val lab = scope.lab("tbl:").headOption.map("{#%s}".format(_)).getOrElse("")
			(head ++ body :+ ": %s %s".format(cap, lab)).mkString("\n")
		}
	}
}

object TeXt {
	def process(args: Array[String]): String = {
		val pdfs = args.lastOption.map(_.split('.').dropRight(1).mkString("."))
		val docs = args.toSeq.map(path => Using(Source.fromFile(path))(_.getLines.mkString("\n")).get)
		val asts = docs.map(TeXPEGs.parseTeX(_)).map(ast => ast.cvt(ast))
		val sout = new StringWriter()
		val pout = new PrintWriter(sout)
		asts.lastOption.foreach(_ => pout.println(TeXPEGs.parseTeX("\\@maintitle").cvt(null).str(null)))
		asts.lastOption.foreach(_.body.lastOption.map(ast => ast.str(ast)).foreach(pout.println))
		sout.toString
	}
	def main(args: Array[String]): Unit = println(process(args))
}
