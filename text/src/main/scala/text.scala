package text

import scala.io.Source
import scala.util.Using

object TeXt {
	def main(args: Array[String]): Unit = println(process(args))
	def process(args: Array[String]): String = {
		val pdfs = args.lastOption.map(_.split('.').dropRight(1).mkString("."))
		val docs = args.toSeq.map(path => Using(Source.fromFile(path))(_.getLines().mkString("\n")).get)
		val text = docs.map(tex2tex.TeXPEGs.parseTeX(_)).mkString("\n")
		val tree = tex2tex.TeXPEGs.parseTeX(text).toMD
		val body = tree.cvt(tree)(false).str(tree)(false)
		val head = tex2tex.TeXPEGs.parseTeX("\\@maintitle").toMD.cvt(null)(false).str(null)(false)
		"%s%n===%n".format(head) + body
	}
}
