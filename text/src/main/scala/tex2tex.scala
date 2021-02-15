package tex2tex

import scala.collection.mutable.{Buffer, Map => MutableMap, Queue}
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

object TeXPEGs extends RegexParsers with PackratParsers {
	lazy val id = """[@A-Za-z]+\*?""".r
	lazy val yen = "\\" ~> id ^^ YenTeX
	lazy val cmt = "%" ~ ".*".r ^^ (_ => StrTeX(""))
	lazy val str = """[^\\\{\}\[\]%\$]+""".r ^^ StrTeX
	lazy val bra = "{" ~> doc <~ "}" ^^ ArgTeX
	lazy val sqb = "[" ~> doc <~ "]" ^^ OptTeX
	lazy val esc = "\\" ~> """[\\{}_&%\$#\|;!, ]""".r ^^ EscTeX
	lazy val env = ("\\begin{" ~> id <~ "}") ~ arg ~ doc <~ ("\\end{" ~> id <~ "}") ^^ {
		case name ~ args ~ body => EnvAppTeX(name, args, body)
	}
	lazy val cmd = not("\\end{") ~> yen ~ arg ^^ {
		case name ~ args => CmdAppTeX(name, args)
	}
	lazy val mat = "$" ~> (esc | cmd | bra | sqb | str).* <~ "$" ^^ DocTeX ^^ MatTeX
	lazy val arg: Parser[DocTeX] = (yen.+ ~ (sqb | bra).*) ^^ {
		case y ~ args => DocTeX(y ++ args)
	} | ((sqb | bra).* ^^ DocTeX)
	lazy val doc: Parser[DocTeX] = (cmt | esc | tim | env | vrb | cmd | bra | sqb | str | mat).* ^^ DocTeX
	lazy val vrb = vrb1 | vrb2
	lazy val vrb1 = ("\\verb#" ~> "[^#]*".r <~ "#") ^^ {
		case quoted => VrbTeX("#", quoted)
	}
	lazy val vrb2 = ("\\verb|" ~> """[^\|]*""".r <~ "|") ^^ {
		case quoted => VrbTeX("|", quoted)
	}
	lazy val tim = "\\begin{Verbatim}" ~> arg ~ (not("\\end") ~> "[\\S\\s]".r).* <~ "\\end{Verbatim}" ^^ {
		case args ~ lst => LstTeX(args.body.head, lst.mkString)
	}
	def parseTeX(str: String): DocTeX = parseAll(doc, str) match {
		case Success(ast, _) => ast
		case Failure(msg, next) => sys.error(s"${msg}\nat${next.pos}${next.pos.longString}")
		case Error(msg, next) => sys.error(s"${msg}\nat${next.pos}${next.pos.longString}")
	}
	override def skipWhitespace = false
}

object ParamPEGs extends RegexParsers with PackratParsers {
	lazy val m = "m" ^^ ParamM
	lazy val O = "O" ~> ("{" ~> str <~ "}") ^^ ParamO
	lazy val str = """[^\}]*""".r ^^ StrTeX
	lazy val params = (m | O).*
	def parseAll(str: String): Seq[Param] = parseAll(params, str) match {
		case Success(ast, _) => ast
		case fail: NoSuccess => sys.error(fail.msg)
	}
	override def skipWhitespace = true
}

object Binds {
	val binds = MutableMap[YenTeX, NewCmdTeX]()
	def get(name: YenTeX) = binds.get(name)
}

trait TeX {
	override def toString(): String
	def asArg = this.asInstanceOf[ArgTeX]
	def asOpt = this.asInstanceOf[OptTeX]
	def asYen = this.asInstanceOf[YenTeX]
	def peel = toString()
}

case class CmdAppTeX(name: YenTeX, args: DocTeX) extends TeX {
	override def toString() = Binds.get(name) match {
		case Some(cmd) => cmd.expand(this)
		case None => name match {
			case YenTeX("bm") => "%s%s".format(YenTeX("boldsymbol"), args)
			case YenTeX("coloneqq") => ":=%s".format(args)
			case YenTeX("hfill") => args.toString()
			case YenTeX("displaystyle") => args.toString()
			case YenTeX("mathchoice") => args.body.head.asArg.peel
			case YenTeX("NewDocumentCommand") => NewCmdTeX(name, args).toString()
			case name => "%s%s".format(name, args)
		}
	}
}

trait Param

case class ParamM(partype: String) extends Param
case class ParamO(default: StrTeX) extends Param

case class NewCmdTeX(cmd: YenTeX, args: DocTeX) extends TeX {
	Binds.binds(args.body.head.asYen) = this
	/**
	 * command body
	 */
	def body = args.body.last.asArg.peel

	/**
	 * command body that can be used as a formatted String
	 */
	val BODY = body.replaceAll("""#(\d)""", """%$1\$s""")	

	/**
	 * command parameters
	 */
	val pars = ParamPEGs.parseAll(args.body(1).asArg.peel)

	/**
	 * process command arguments
	 *
	 * @return (used arguments, unused arguments)
	 */
	def passArgs(app: CmdAppTeX): (Seq[TeX], Seq[TeX]) = {
		// all arguments explicitly specified
		if(app.args.body.size >= pars.size) {
			app.args.body.splitAt(pars.size)
		} else {
			val buf = Buffer[TeX]()
			val que = Queue[TeX](app.args.body:_*)
			for(par <- this.pars) par match {
				case ParamM(m) => buf += que.dequeue()
				case ParamO(default) => buf += default
			}
			(buf.toSeq, que.toSeq)
		}
	}

	/**
	 * expand this macro into the specified expression
	 */
	def expand(app: CmdAppTeX): String = {
		var tex = expandOnce(app)
		var exp = ""
		do {
			exp = tex
			tex = TeXPEGs.parseTeX(tex).toString()
		} while(tex != exp)
		tex
	}

	/**
	 * expand this macro into the specified expression
	 */
	def expandOnce(app: CmdAppTeX): String = {
		val (args, rest) = this.passArgs(app)
		val vals = args.map(_.peel)
		this.BODY.format(vals:_*).concat(rest.mkString)
	}

	override def toString() = cmd.toString().concat(args.toString())
}

case class EnvAppTeX(name: String, args: DocTeX, body: TeX) extends TeX {
	def str = """\begin{%1$s}%2$s%3$s\end{%1$s}""".format(name, args, body)
	override def toString() = name match {
		case "equation" => this.str.replace("\n", " ").replace("\r", "").trim
		case _ => this.str
	}
}

case class YenTeX(text: String) extends TeX {
	override def toString() = """\""".concat(text)
}

case class OptTeX(body: TeX) extends TeX {
	override def toString() = "[%s]".format(body)
	override def peel = body.toString()
}

case class ArgTeX(body: TeX) extends TeX {
	override def toString() = "{%s}".format(body)
	override def peel = body.toString()
}

case class StrTeX(text: String) extends TeX {
	override def toString() = text
}

case class EscTeX(char: String) extends TeX {
	override def toString() = """\""".concat(char)
}

case class VrbTeX(del: String, body: String) extends TeX {
	override def toString() = s"\\verb${del}${body}${del}"
}

case class LstTeX(lang: TeX, body: String) extends TeX {
	override def toString() = s"""\\begin{Verbatim}${lang}${body}\\end{Verbatim}"""
}

case class MatTeX(body: TeX) extends TeX {
	override def toString() = s"$$${body.toString().trim}$$"
}

case class DocTeX(body: Seq[TeX]) extends TeX {
	override def toString() = body.mkString
}
