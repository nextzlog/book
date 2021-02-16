package middle

import decode._

trait ST {
	def toMD: MD
}

case class Cmd(name: String, to: DocMD => CmdBodyMD) {
	def toMD(args: DocMD): CmdBodyMD = to(args)
}

case class Env(name: String, to: (DocMD, MD, encode.TeX) => EnvBodyMD) {
	def toMD(args: DocMD, body: MD, tex: encode.TeX): EnvBodyMD = to(args, body, tex)
}

case class CmdAppST(name: YenST, args: DocST) extends ST {
	override def toMD = Cmds.find(name.toMD).map(_.toMD(args.toMD)).getOrElse(EmptyMD)
}

case class EnvAppST(name: StrST, args: DocST, body: ST, tex: encode.TeX) extends ST {
	override def toMD = Envs.find(name.toMD).map(_.toMD(args.toMD, body.toMD, tex)).getOrElse(EmptyMD)
}

case class YenST(text: String) extends ST {
	override def toMD = YenMD(text)
}

case class OptST(body: ST) extends ST {
	override def toMD = OptMD(body.toMD)
}

case class ArgST(body: ST) extends ST {
	override def toMD = ArgMD(body.toMD)
}

case class StrST(text: String) extends ST {
	override def toMD = StrMD(text)
}

case class EscST(char: String) extends ST {
	override def toMD = EscMD(char)
}

case class VrbST(body: String) extends ST {
	override def toMD = VrbMD(body)
}

case class LstST(lang: ST, body: String) extends ST {
	override def toMD = LstMD(lang.toMD, body)
}

case class MatST(body: encode.TeX) extends ST {
	override def toMD = MatMD(body)
}

case class DocST(body: Seq[ST]) extends ST {
	override def toMD = DocMD(body.map(_.toMD))
}

class BaseCmds(cmds: Cmd*) {
	val table = cmds.map(cmd => cmd.name.toLowerCase -> cmd).toMap
	def find(name: YenMD) = table.get(name.text.toLowerCase)
}

class BaseEnvs(envs: Env*) {
	val table = envs.map(env => env.name.toLowerCase -> env).toMap
	def find(name: StrMD) = table.get(name.text.toLowerCase)
}

object Cmds extends BaseCmds(
	Cmd("BottomRule", OutputNothingCmd(_)),
	Cmd("Caption", Caption(_)),
	Cmd("Centering", OutputNothingCmd(_)),
	Cmd("Chapter", Chapter(_)),
	Cmd("DocumentClass", OutputNothingCmd(_)),
	Cmd("EqRef", EqRef(_)),
	Cmd("IfPackageLoaded", OutputNothingCmd(_)),
	Cmd("IncludeGraphics", IncludeGraphics(_)),
	Cmd("Label", Label(_)),
	Cmd("LetLtxMacro", OutputNothingCmd(_)),
	Cmd("LstDefineLanguage", OutputNothingCmd(_)),
	Cmd("LstNewEnvironment", OutputNothingCmd(_)),
	Cmd("MakeTitle", OutputNothingCmd(_)),
	Cmd("MidRule", new MidRule(_)),
	Cmd("NewDocumentEnvironment", OutputNothingCmd(_)),
	Cmd("Quad", OutputNothingCmd(_)),
	Cmd("Ref", Ref(_)),
	Cmd("RenewCommand", OutputNothingCmd(_)),
	Cmd("RenewDocumentCommand", OutputNothingCmd(_)),
	Cmd("RenewDocumentEnvironment", OutputNothingCmd(_)),
	Cmd("RequirePackage", OutputNothingCmd(_)),
	Cmd("Section", Section(_)),
	Cmd("SubFigRef", SubFigRef(_)),
	Cmd("SubFloat", SubFloat(_)),
	Cmd("SubRef", SubRef(_)),
	Cmd("SubSection", SubSection(_)),
	Cmd("TabRef", TabRef(_)),
	Cmd("TableOfContents", OutputNothingCmd(_)),
	Cmd("TextBf", TextBf(_)),
	Cmd("TextIt", TextIt(_)),
	Cmd("TextTt", TextTt(_)),
	Cmd("Title", Title(_)),
	Cmd("TopRule", OutputNothingCmd(_)),
	Cmd("UsePackage", OutputNothingCmd(_))
)

object Envs extends BaseEnvs(
	Env("Document", Document(_, _, _)),
	Env("Equation", Equation(_, _, _)),
	Env("Figure", Figure(_, _, _)),
	Env("Table", Table(_, _, _)),
	Env("Tabular", Tabular(_, _, _))
)
