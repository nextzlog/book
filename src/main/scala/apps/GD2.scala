package apps

import java.io.PrintStream
import scala.language.postfixOps
import scala.sys.process._

object GD2 {
	def main() {
		println(test("PlainSGD", ()=>new PlainSGD))
		println(test("AdaDelta", ()=>new AdaDelta))
		("python src/main/python/GD2.py PlainSGD AdaDelta" #&& "rm PlainSGD.dat AdaDelta.dat" #&& "open plot.svg" !)
	}
	def test(name: String, sgd: ()=>SGD): Int = {
		val x = sgd()
		val y = sgd()
		x.w = 1.0
		y.w = 0.0005
		val out = new PrintStream(name + ".dat")
		var cnt = 0
		do {
			cnt += 1
			out.println("%f,%f".format(x.w, y.w))
			x.update(+x.w)
			y.update(-y.w)
		} while(x.w.abs < 2 && y.w.abs < 2)
		out.println("%f,%f".format(x.w, y.w))
		out.close
		cnt
	}
}
