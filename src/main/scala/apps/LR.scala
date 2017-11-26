package apps

import java.io.PrintStream

class Regression(e: Double, XY: Seq[(Double, Double)], p: Seq[Double=>Double]) {
	val w = Array.fill[Double](p.size)(0)
	def apply(x: Double) = w.zip(p.map(_(x))).map{case (w,x)=>w*x}.sum
	for(n<-1 to 1000; (x,y) <- XY; k<-0 until p.size) w(k) += e * (y-this(x)) * p(k)(x)
}

object LR {
	val range = -10.0 to 10.0 by 0.05
	def main() {
		val pts = range.map(x=>(x.toDouble, math.pow(x,3) + 50 * util.Random.nextGaussian))
		val reg = new Regression(0.000001, pts, 0.to(3).map(k=>(x: Double)=>math.pow(x,k)))
		val out = new PrintStream(s"dist.dat")
		for((x,y) <- pts) out.println("%f,%f".format(x,y))
		out.close
		exec.Python.run("LR", reg.w:_*)
	}
}
