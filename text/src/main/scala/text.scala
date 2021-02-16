package text

import scala.io.Source
import scala.util.Using
import tex2tex.TeXPEGs

object TeXt {
	def main(args: Array[String]): Unit = println(process(args))
	def process(args: Array[String]): String = {
		val texs = args.lastOption.map(_.split('.').dropRight(1).mkString("."))
		val docs = args.map(path => Source.fromFile(path).mkString).mkString
		val text = docs.replace("\\maketitle", "\\title{\\@maintitle}")
		val tree = TeXPEGs.parseTeX(TeXPEGs.parseTeX(text).eval).toMD
		tree.cvt(tree)(false).str(tree)(false)
	}
}
