package apps

import java.io.PrintStream

class Regression(e: Double, XY: Seq[(Double, Double)], p: Seq[Double=>Double]) {
	val w = Array.fill[Double](p.size)(0)
	def apply(x: Double) = w.zip(p.map(_(x))).map{case (w,x)=>w*x}.sum
	for(n<-1 to 5000; (x,y) <- XY; k<-0 until p.size) w(k) += e * (y-this(x)) * p(k)(x)
}

object LR {
	val range = -10.0 to 10.0 by 0.05
	def base1: Unit = {
		val pts = range.map(x=>(x.toDouble, math.pow(x,3) - 3 * x + 50 * util.Random.nextGaussian))
		val reg = new Regression(0.000001, pts, 0.to(3).map(k=>x=>math.pow(x,k)))
		val out = new PrintStream(s"dist1.dat")
		for((x,y) <- pts) out.println("%f,%f".format(x,y))
		out.close
		exec.Python.run("LR", 1 +: reg.w: _*)
	}
	def base2: Unit = {
		def gauss(x: Double, m: Double, s: Double) = math.exp(-(x - m) * (x - m) / 2 / s / s)
		val pts = range.map(x=>(x.toDouble, -300 * gauss(x, -5, 1) + 600 * gauss(x, 5, 1) + 50 * util.Random.nextGaussian))
		val reg = new Regression(0.0001, pts, Seq(x => gauss(x, -5, 1), x => gauss(x, 5, 1)))
		val out = new PrintStream(s"dist2.dat")
		for((x,y) <- pts) out.println("%f,%f".format(x,y))
		out.close
		exec.Python.run("LR", 2 +: reg.w: _*)
	}
	def main(): Unit = {
		base1
		base2
	}
}
