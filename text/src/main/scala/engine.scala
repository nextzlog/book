package engine

import scala.io.Source
import scala.util.Using
import encode.TeXPEGs

object TeXt {
	def main(args: Array[String]): Unit = println(process(args))
	def process(args: Array[String]): String = {
		val raws = args.map(path => Source.fromFile(path).mkString)
		val docs = raws.map(_.replace("\\maketitle", "\\title{\\@maintitle}"))
		val tree = docs.map(text => TeXPEGs.parseTeX(text).eval)
		val text = tree.map(text => TeXPEGs.parseTeX(text).toST.toMD)
		text.lastOption.map(md => md.str(md)).getOrElse("")
	}
}
