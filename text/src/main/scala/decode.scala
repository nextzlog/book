package decode

import encode.TeX

trait MD {
	def str(scope: MD): String
	def lab(scope: MD): Seq[String]
	def cap(scope: MD): Seq[String]
	def mid: Boolean = false
}

abstract class LeafMD extends MD {
	override def lab(scope: MD): Seq[String] = Seq()
	override def cap(scope: MD): Seq[String] = Seq()
}

abstract class TreeMD(children: MD*) extends MD {
	override def mid = children.exists(_.mid)
	def lab(scope: MD) = children.map(_.lab(scope)).flatten
	def cap(scope: MD) = children.map(_.cap(scope)).flatten
}

abstract class CmdBodyMD(args: DocMD) extends TreeMD(args)

abstract class EnvBodyMD(body: MD) extends TreeMD(body)

case class YenMD(text: String) extends LeafMD {
	override def str(scope: MD) = text
}

case class OptMD(body: MD) extends TreeMD(body) {
	override def str(scope: MD) = body.str(scope)
}

case class ArgMD(body: MD) extends TreeMD(body) {
	override def str(scope: MD) = body.str(scope)
}

case class StrMD(text: String) extends LeafMD {
	override def str(scope: MD) = text.replace('~', ' ')
}

case class EscMD(char: String) extends LeafMD {
	override def str(scope: MD) = char match {
		case "\\" => "\n"
		case _ => char
	}
}

object EmptyMD extends LeafMD {
	override def str(scope: MD) = ""
}

case class VrbMD(body: String) extends LeafMD {
	override def str(scope: MD) = "`%s`".format(body.replace("|", "\\|"))
}

case class LstMD(lang: MD, body: String) extends LeafMD {
	override def str(scope: MD) = s"```${lang.str(scope)}${body}```"
}

case class MatMD(body: TeX) extends LeafMD {
	override def str(scope: MD) = s"$$${body}$$"
}

case class DocMD(body: Seq[MD]) extends TreeMD(body: _*) {
	override def str(scope: MD) = body.map(_.str(scope)).mkString
}

trait Labels {
	def format(label: String) = label.split(":", 2).toSeq match {
		case Seq("chap", lab) => "sec:".concat(lab)
		case Seq("sect", lab) => "sec:".concat(lab)
		case Seq("fig",  lab) => "fig:".concat(lab)
		case Seq("tab",  lab) => "tbl:".concat(lab)
		case Seq("eq",   lab) => "eq:" .concat(lab)
		case other => other.mkString(":")
	}
	def label(args: DocMD, scope: MD) = format(args.body.head.str(scope))
}

case class Label(args: DocMD) extends CmdBodyMD(args) with Labels {
	override def lab(scope: MD) = Seq(label(args, scope))
	override def str(scope: MD) = ""
}

case class Ref(args: DocMD) extends CmdBodyMD(args) with Labels {
	override def str(scope: MD) = "[@%s]".format(label(args, scope))
}

case class EqRef(args: DocMD) extends CmdBodyMD(args) with Labels {
	override def str(scope: MD) = "[@eq:%s]".format(label(args, scope))
}

case class TabRef(args: DocMD) extends CmdBodyMD(args) with Labels {
	override def str(scope: MD) = "[@%s]".format(label(args, scope))
}

case class SubRef(args: DocMD) extends CmdBodyMD(args) with Labels {
	override def str(scope: MD) = "[@%s]".format(label(args, scope))
}

case class SubFigRef(args: DocMD) extends CmdBodyMD(args) with Labels {
	override def str(scope: MD) = "[@fig:%s]".format(label(args, scope))
}

case class Caption(args: DocMD) extends CmdBodyMD(args) {
	override def cap(scope: MD) = Seq(args.body.head.str(scope))
	override def str(scope: MD) = ""
}

case class IncludeGraphics(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = {
		val path = args.body.last.str(scope).replaceAll(".eps$", ".svg")
		val cap = scope.cap(scope).headOption.getOrElse("")
		val lab = scope.lab(scope).headOption.map("{#%s}".format(_)).getOrElse("")
		"![%s](%s)%s".format(cap, path, lab)
	}
}

case class SubFloat(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = args.body.last.str(this)
	override def cap(scope: MD) = Seq(args.body.head.str(this))
}

case class Chapter(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = {
		val tag = s"# ${args.str(scope)}"
		val lab = this.lab(scope).headOption.map(" {#%s}".format(_)).getOrElse("")
		tag.concat(lab)
	}
}

case class Section(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = {
		val tag = s"## ${args.str(scope)}"
		val lab = this.lab(scope).headOption.map(" {#%s}".format(_)).getOrElse("")
		tag.concat(lab)
	}
}

case class SubSection(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = {
		val tag = s"### ${args.str(scope)}"
		val lab = this.lab(scope).headOption.map(" {#%s}".format(_)).getOrElse("")
		tag.concat(lab)
	}
}

case class TextBf(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"**${args.str(scope)}**"
}

case class TextIt(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"*${args.str(scope)}*"
}

case class Title(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = s"${args.str(scope)}\n===\n"
}

case class TextTt(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = args.str(scope)
}

case class MathChoice(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = args.body.head.str(scope)
}

case class OutputNothingCmd(args: DocMD) extends CmdBodyMD(args) {
	override def str(scope: MD) = ""
}

case class OutputNothingEnv(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = ""
}

class MidRule(args: DocMD) extends OutputNothingCmd(args) {
	override def mid = true
}

case class Document(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = body.str(this)
}

case class Equation(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = {
		val lab = this.lab(scope).headOption.map(" {#%s}".format(_)).getOrElse("")
		s"$$$$${tex.view}$$$$${lab}"
	}
}

case class Figure(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = body.str(this).trim match {
		case body if cap(scope).size > 1 => s"<div id='${lab(scope).last}'>\n$body\n\n${cap(scope).last}\n</div>"
		case body => body
	}
}

case class Table(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = body.str(this).trim
}

case class Tabular(args: DocMD, body: MD, tex: TeX) extends EnvBodyMD(body) {
	override def str(scope: MD) = {
		val ncol = args.body.head.str(scope).count(_.toChar.isLetter)
		val rows = body.str(scope).replace(" & ", " | ").replace("\\\\", "").trim.linesIterator.toSeq
		val head = Seq(if(body.mid) rows.head else Seq.fill(ncol)("-").mkString("|"))
		val rule = Seq.fill(ncol)("---").mkString("|")
		val tail = if(body.mid) rows.tail else rows
		val cap = scope.cap(scope).headOption.getOrElse("")
		val lab = scope.lab(scope).headOption.map("{#%s}".format(_)).getOrElse("")
		val data = (head :+ rule) ++ tail.filterNot(_.trim.isEmpty)
		(data.map("|%s|".format(_)) :+ ": %s %s".format(cap, lab)).mkString("\n")
	}
}
