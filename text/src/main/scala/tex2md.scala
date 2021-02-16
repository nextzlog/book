package tex2md

case class Scope(cmds: Seq[CmdMD], envs: Seq[EnvMD], out: Option[Scope]) {
	val cmdsTable = cmds.map(tex => tex.name -> tex).to(collection.mutable.Map)
	val envsTable = envs.map(tex => tex.name -> tex).to(collection.mutable.Map)
	def install(cmd: CmdMD) = cmdsTable.getOrElseUpdate(cmd.name, cmd)
	def install(env: EnvMD) = envsTable.getOrElseUpdate(env.name, env)
	def cmd(name: YenMD): Option[CmdMD] = cmdsTable.get(name).orElse(out.map(_.cmd(name)).flatten)
	def env(name: String): Option[EnvMD] = envsTable.get(name).orElse(out.map(_.env(name)).flatten)
}

object Root extends Scope(Seq(
	LabelCmdMD,
	RefCmdMD,
	EqRefCmdMD,
	TabRefCmdMD,
	SubRefCmdMD,
	SubFigRefCmdMD,
	CaptionCmdMD,
	IncludeGraphicsCmdMD,
	SubFloatCmdMD,
	ChapterCmdMD,
	SectionCmdMD,
	SubSectionCmdMD,
	TextBfCmdMD,
	TextItCmdMD,
	TextTtCmdMD,
	TitleCmdMD,
	HFillCmdMD,
	TopRuleCmdMD,
	MidRuleCmdMD,
	BottomRuleCmdMD,
	BmCmdMD,
	ColonEqqCmdMD,
	MathChoiceCmdMD,
	CenteringCmdMD,
	QuadCmdMD,
	DefCmdMD,
	GDefCmdMD,
	LetCmdMD,
	LetLtxMacroCmdMD,
	DocumentClassCmdMD,
	MakeTitleCmdMD,
	TableOfContentsCmdMD,
	UsePackageCmdMD,
	IfPackageLoadedCmdMD,
	RequirePackageCmdMD,
	LstDefineLanguageCmdMD,
	LstNewEnvironmentCmdMD,
	NewCommandCmdMD,
	RenewCommandCmdMD,
	NewDocumentCommandCmdMD,
	RenewDocumentCommandCmdMD,
	NewDocumentEnvironmentCmdMD,
	RenewDocumentEnvironmentCmdMD,
	DeclareMathOperatorCmdMD
), Seq(
	DocumentEnvMD,
	EquationEnvMD,
	FigureEnvMD,
	TabularEnvMD
), None)

trait MD {
	def cvt(scope: MD)(implicit isMath: Boolean): MD
	def str(scope: MD)(implicit isMath: Boolean): String
	def peel(scope: MD)(implicit isMath: Boolean) = str(scope) match {
		case str if str.startsWith("{") && str.endsWith("}") => str.tail.init
		case str if str.startsWith("[") && str.endsWith("]") => str.tail.init
		case str => str
	}
	def midruled: Boolean = false
	def lab(prefix: String): Seq[String]
	def cap: Seq[String]
}

abstract class LeafMD extends MD {
	override def cvt(scope: MD)(implicit isMath: Boolean): MD = this
	override def lab(prefix: String): Seq[String] = Seq()
	override def cap: Seq[String] = Seq()
}

abstract class TreeMD(children: MD*) extends MD {
	override def midruled = children.exists(_.midruled)
	def lab(prefix: String) = children.map(_.lab(prefix)).flatten
	def cap = children.map(_.cap).flatten
}

abstract class CmdMD(raw: String) {
	def midruled = false
	def name = YenMD(raw.replaceAll("""\\""", ""))
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean): CmdBodyMD
}

class CmdBodyMD(app: CmdAppMD, cmd: CmdMD) extends TreeMD(app.args) {
	override def midruled = cmd.midruled
	override def cvt(scope: MD)(implicit isMath: Boolean): MD = cmd(CmdAppMD(cmd.name, app.args.cvt(scope)), scope)
	override def str(scope: MD)(implicit isMath: Boolean) = ""
}

abstract class EnvMD(val name: String) {
	def apply(app: EnvAppMD, scope: MD)(implicit isMath: Boolean): EnvBodyMD
}

class EnvBodyMD(app: EnvAppMD, env: EnvMD) extends TreeMD(app) {
	override def cvt(scope: MD)(implicit isMath: Boolean): MD = env(EnvAppMD(env.name, app.args.cvt(this), app.body.cvt(scope)), this)
	override def str(scope: MD)(implicit isMath: Boolean) = app.body.str(scope)
}

case class CmdAppMD(name: YenMD, args: DocMD) extends TreeMD(args) {
	override def cvt(scope: MD)(implicit isMath: Boolean) = Root.cmd(name).map(_ (this, scope).cvt(scope)).getOrElse(CmdAppMD(name, args.cvt(scope)))
	override def str(scope: MD)(implicit isMath: Boolean) = name.str(scope).concat(args.str(scope))
}

case class EnvAppMD(name: String, args: DocMD, body: MD) extends TreeMD(args, body) {
	override def cvt(scope: MD)(implicit isMath: Boolean) = Root.env(name).map(_ (this, this).cvt(this)).getOrElse(EnvAppMD(name, args.cvt(this), body.cvt(this)))
	override def str(scope: MD)(implicit isMath: Boolean) = isMath match {
		case true => """\begin{%1$s}%2$s%3$s\end{%1$s}""".format(name, args.str(scope), body.str(scope))
		case false => body.str(this)
	}
}

case class YenMD(text: String) extends LeafMD {
	override def cvt(scope: MD)(implicit isMath: Boolean) = CmdAppMD(this, DocMD(Seq())).cvt(scope)
	override def str(scope: MD)(implicit isMath: Boolean) = """\""".concat(text)
}

case class OptMD(body: MD) extends TreeMD(body) {
	override def cvt(scope: MD)(implicit isMath: Boolean) = OptMD(body.cvt(scope))
	override def str(scope: MD)(implicit isMath: Boolean) = """[%s]""".format(body.str(scope))
}

case class ArgMD(body: MD) extends TreeMD(body) {
	override def cvt(scope: MD)(implicit isMath: Boolean) = ArgMD(body.cvt(scope))
	override def str(scope: MD)(implicit isMath: Boolean) = """{%s}""".format(body.str(scope))
}

case class StrMD(text: String) extends LeafMD {
	override def str(scope: MD)(implicit isMath: Boolean) = text.replace('~', ' ')
}

case class EscMD(char: String) extends LeafMD {
	override def str(scope: MD)(implicit isMath: Boolean) = char match {
		case _ if isMath => "\\".concat(char)
		case "\\" if !isMath => "\n"
		case _ if !isMath => char
	}
}

case class VrbMD(body: String) extends LeafMD {
	override def str(scope: MD)(implicit isMath: Boolean) = "`%s`".format(body.replace("|", "\\|"))
}

case class LstMD(lang: MD, body: String) extends LeafMD {
	override def str(scope: MD)(implicit isMath: Boolean) = s"```${lang.peel(scope)}${body}```"
}

case class MatMD(body: MD) extends TreeMD(body) {
	override def cvt(scope: MD)(implicit isMath: Boolean) = MatMD(body.cvt(this)(true))
	override def str(scope: MD)(implicit isMath: Boolean) = s"$$${body.str(this)(true).trim}$$"
}

case class DocMD(body: Seq[MD]) extends TreeMD(body: _*) {
	override def cvt(scope: MD)(implicit isMath: Boolean) = DocMD(body.map(_.cvt(scope)))
	override def str(scope: MD)(implicit isMath: Boolean) = body.map(_.str(scope)).mkString
	override def peel(scope: MD)(implicit isMath: Boolean) = body.map(_.peel(scope)).mkString
}

object StandardLabelFormat {
	def apply(label: String) = label.split(":", 2).toSeq match {
		case Seq("chap", lab) => "sec:".concat(lab)
		case Seq("sect", lab) => "sec:".concat(lab)
		case Seq("tab", lab) => "tbl:".concat(lab)
		case other => other.mkString(":")
	}
}

object LabelCmdMD extends CmdMD("label") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def lab(prefix: String) = Seq(label).filter(_.startsWith(prefix))
	}
}

object RefCmdMD extends CmdMD("ref") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def str(scope: MD)(implicit isMath: Boolean) = "[@%s]".format(label)
	}
}

object EqRefCmdMD extends CmdMD("eqref") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def str(scope: MD)(implicit isMath: Boolean) = "[@eq:%s]".format(label)
	}
}

object TabRefCmdMD extends CmdMD("tabref") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		val label = StandardLabelFormat("tbl:".concat(app.args.body.head.peel(scope)))
		override def str(scope: MD)(implicit isMath: Boolean) = scope.lab(label.concat(":")).padTo(1, label).map("[@%s]".format(_)).mkString(", ")
	}
}

object SubRefCmdMD extends CmdMD("subref") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		val label = StandardLabelFormat(app.args.body.head.peel(scope))
		override def str(scope: MD)(implicit isMath: Boolean) = "[@%s]".format(label)
	}
}

object SubFigRefCmdMD extends CmdMD("subfigref") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		val label = StandardLabelFormat(app.args.body.map(_.peel(scope)).mkString(":"))
		override def str(scope: MD)(implicit isMath: Boolean) = "[@fig:%s]".format(label)
	}
}

object CaptionCmdMD extends CmdMD("caption") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def cap = Seq(app.args.body.head.peel(scope))
	}
}

object IncludeGraphicsCmdMD extends CmdMD("includegraphics") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		val path = app.args.body.last.peel(scope).replaceAll(".eps$", ".svg")
		override def str(scope: MD)(implicit isMath: Boolean) = {
			val cap = scope.cap.headOption.getOrElse("")
			val lab = scope.lab("fig:").headOption.map("{#%s}".format(_)).getOrElse("")
			"![%s](%s)%s".format(cap, path, lab)
		}
	}
}

object SubFloatCmdMD extends CmdMD("subfloat") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def cvt(scope: MD)(implicit isMath: Boolean) = apply(CmdAppMD(name, app.args.cvt(this)), this)
		override def str(scope: MD)(implicit isMath: Boolean) = app.args.body.last.peel(this)
		override def cap = Seq(app.args.body.head.peel(this))
	}
}

object ChapterCmdMD extends CmdMD("chapter") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = {
			val tag = s"# ${app.args.peel(scope)}"
			val lab = this.lab("sec:").headOption.map(" {#%s}".format(_)).getOrElse("")
			tag.concat(lab)
		}
	}
}

object SectionCmdMD extends CmdMD("section") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = {
			val tag = s"## ${app.args.peel(scope)}"
			val lab = this.lab("sec:").headOption.map(" {#%s}".format(_)).getOrElse("")
			tag.concat(lab)
		}
	}
}

object SubSectionCmdMD extends CmdMD("subsection") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = {
			val tag = s"### ${app.args.peel(scope)}"
			val lab = this.lab("sec:").headOption.map(" {#%s}".format(_)).getOrElse("")
			tag.concat(lab)
		}
	}
}

object TextBfCmdMD extends CmdMD("textbf") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = isMath match {
			case true => s"""\\textbf{${app.args.peel(scope)}}"""
			case false => s"**${app.args.peel(scope)}**"
		}
	}
}

object TextItCmdMD extends CmdMD("textit") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = isMath match {
			case true => s"""\\textit{${app.args.peel(scope)}}"""
			case false => s"*${app.args.peel(scope)}*"
		}
	}
}

object TitleCmdMD extends CmdMD("title") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = s"${app.args.peel(scope)}\n===\n"
	}
}

object TextTtCmdMD extends CmdMD("texttt") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = isMath match {
			case true => s"""\\texttt{${app.args.peel(scope)}}"""
			case false => app.args.peel(scope)
		}
	}
}

object BmCmdMD extends CmdMD("bm") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = s"""\\boldsymbol%s""".format(app.args.str(scope))
	}
}

object ColonEqqCmdMD extends CmdMD("coloneqq") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = """:="""
	}
}

object MathChoiceCmdMD extends CmdMD("mathchoice") {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = app.args.body.head.str(scope)
	}
}

class OutputNothingCmdMD(name: String) extends CmdMD(name) {
	def apply(app: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(app, this)
}

class OutputNothingEnvMD(name: String) extends EnvMD(name) {
	def apply(app: EnvAppMD, scope: MD)(implicit isMath: Boolean) = new EnvBodyMD(app, this)
}

object LetLtxMacroCmdMD extends CmdMD("LetLtxMacro") {
	def apply(outer: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(outer, this) {
		val newName = outer.args.body.head.peel(scope)
		val oldName = outer.args.body.last.peel(scope)
		override def cvt(scope: MD)(implicit isMath: Boolean) = {
			Root.install(new CmdMD(newName) {
				def apply(inner: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(inner, this) {
					val value = inner.args.str(scope)
					override def cvt(scope: MD)(implicit isMath: Boolean) = this
					override def str(scope: MD)(implicit isMath: Boolean) = oldName.concat(value)
				}
			})
			this
		}
	}
}

class BaseNewCommandCmdMD(name: String) extends CmdMD(name) {
	def apply(outer: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(outer, this) {
		val name = outer.args.body.head.peel(scope)
		override def cvt(scope: MD)(implicit isMath: Boolean) = {
			Root.install(new CmdMD(name) {
				def apply(inner: CmdAppMD, scope: MD)(implicit isMath: Boolean) = new CmdBodyMD(inner, this) {
					override def cvt(scope: MD)(implicit isMath: Boolean) = {
						val data = outer.args.body.last.peel(scope)(true)
						val narg = """#(\d+)""".r.findAllIn(data).distinct.size
						val text = data.replaceAll("""#(\d)""", """%$1\$s""")
						val (args, tail) = inner.args.body.splitAt(narg)
						val full = args ++ Seq.fill(narg - args.size)(StrMD(""))
						val form = text.format(full.map(_.cvt(scope).peel(scope)): _*)
						val rest = tail.map(_.cvt(scope).peel(scope)).mkString
						tex2tex.TeXPEGs.parseTeX(form.concat(rest)).toMD.cvt(scope)
					}
				}
			})
			this
		}
	}
}

object DocumentClassCmdMD extends OutputNothingCmdMD("documentclass")

object MakeTitleCmdMD extends OutputNothingCmdMD("maketitle")

object TableOfContentsCmdMD extends OutputNothingCmdMD("tableofcontents")

object UsePackageCmdMD extends OutputNothingCmdMD("usepackage")

object RequirePackageCmdMD extends OutputNothingCmdMD("RequirePackage")

object IfPackageLoadedCmdMD extends OutputNothingCmdMD("@ifpackageloaded")

object LstDefineLanguageCmdMD extends OutputNothingCmdMD("lstdefinelanguage")

object LstNewEnvironmentCmdMD extends OutputNothingCmdMD("lstnewenvironment")

object CenteringCmdMD extends OutputNothingCmdMD("centering")

object QuadCmdMD extends OutputNothingCmdMD("quad")

object HFillCmdMD extends OutputNothingCmdMD("hfill")

object TopRuleCmdMD extends OutputNothingCmdMD("toprule")

object MidRuleCmdMD extends OutputNothingCmdMD("midrule") {
	override def midruled = true
}

object BottomRuleCmdMD extends OutputNothingCmdMD("bottomrule")

object DefCmdMD extends BaseNewCommandCmdMD("def")

object GDefCmdMD extends BaseNewCommandCmdMD("gdef")

object LetCmdMD extends BaseNewCommandCmdMD("let")

object NewCommandCmdMD extends BaseNewCommandCmdMD("newcommand")

object RenewCommandCmdMD extends BaseNewCommandCmdMD("renewcommand")

object NewDocumentCommandCmdMD extends BaseNewCommandCmdMD("NewDocumentCommand")

object RenewDocumentCommandCmdMD extends BaseNewCommandCmdMD("RenewDocumentCommand")

object DeclareMathOperatorCmdMD extends BaseNewCommandCmdMD("DeclareMathOperator*")

object NewDocumentEnvironmentCmdMD extends OutputNothingCmdMD("NewDocumentEnvironment")

object RenewDocumentEnvironmentCmdMD extends OutputNothingCmdMD("RenewDocumentEnvironment")

object DocumentEnvMD extends EnvMD("document") {
	def apply(app: EnvAppMD, scope: MD)(implicit isMath: Boolean) = new EnvBodyMD(app, this) {
		override def cvt(scope: MD)(implicit isMath: Boolean) = app.body.cvt(this)
		override def str(scope: MD)(implicit isMath: Boolean) = app.body.str(this)
	}
}

object EquationEnvMD extends EnvMD("equation") {
	def apply(app: EnvAppMD, scope: MD)(implicit isMath: Boolean) = new EnvBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = {
			val exp = app.body.str(this)(true).replace("\n", "").replace("\r", "").trim
			val lab = this.lab("eq:").headOption.map(" {#%s}".format(_)).getOrElse("")
			s"$$$$${exp}$$$$${lab}"
		}
	}
}

object FigureEnvMD extends EnvMD("figure") {
	def apply(app: EnvAppMD, scope: MD)(implicit isMath: Boolean) = new EnvBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = app.body.str(this).trim match {
			case body if cap.size > 1 => s"<div id='${lab("fig:").last}'>\n$body\n\n${cap.last}\n</div>"
			case body => body
		}
	}
}

object TabularEnvMD extends EnvMD("tabular") {
	def apply(app: EnvAppMD, scope: MD)(implicit isMath: Boolean) = new EnvBodyMD(app, this) {
		override def str(scope: MD)(implicit isMath: Boolean) = {
			val ncol = app.args.body.head.str(scope).count(_.toChar.isLetter)
			val rows = app.body.str(scope).replace(" & ", " | ").replace("\\\\", "").trim.linesIterator.toSeq
			val head = Seq(if(app.body.midruled) rows.head else Seq.fill(ncol)("-").mkString("|"))
			val rule = Seq.fill(ncol)("---").mkString("|")
			val body = if(app.body.midruled) rows.tail else rows
			val cap = scope.cap.headOption.getOrElse("")
			val lab = scope.lab("tbl:").headOption.map("{#%s}".format(_)).getOrElse("")
			val data = (head :+ rule) ++ body.filterNot(_.trim.isEmpty)
			(data.map("|%s|".format(_)) :+ ": %s %s".format(cap, lab)).mkString("\n")
		}
	}
}
