package apps

import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps
import scala.sys.process._

class SVM(X: Seq[(Seq[Double],Int)], C: Double, k: (Seq[Double],Seq[Double])=>Double) {
	val data = X.map(Data.tupled)
	var c = 0.0
	case class Data(x: Seq[Double], t: Int) {
		var l = 0.0
	}
	def wx(x: Data) = data.map(d => d.l * d.t * k(x.x,d.x)).sum
	def kkt(x: Data) = (x.t * this(x.x) - 1) match {
		case e if e < -1e-10 => x.l >= C
		case e if e > +1e-10 => x.l <= 0
		case _ => true
	}
	def clip(xi: Data, xj: Data, d: Double): Double = {
		if(d < lower(xi, xj)) return lower(xi, xj)
		if(d > upper(xi, xj)) return upper(xi, xj)
		return if(d.isNaN) 0 else d
	}
	def lower(xi: Data, xj: Data) = xi.t * xj.t match {
		case  1 => math.max(-xi.l, +xj.l - C)
		case -1 => math.max(-xi.l, -xj.l)
	}
	def upper(xi: Data, xj: Data) = xi.t * xj.t match {
		case  1 => math.min(+xj.l, -xi.l + C)
		case -1 => math.min(-xi.l, -xj.l) + C
	}
	def pos = data.filter(_.t == +1).map(wx(_)).min
	def neg = data.filter(_.t == -1).map(wx(_)).max
	while(data.count(!kkt(_)) >= 2) {
		val a = data.find(!kkt(_)).get
		val b = data(util.Random.nextInt(X.size))
		val sub = wx(Data((a.x,b.x).zipped.map(_-_), 0))
		val den = k(a.x,a.x) - 2*k(a.x,b.x) + k(b.x,b.x)
		val del = clip(a, b, -a.t * (sub-a.t+b.t) / den)
		a.l += del
		b.l -= del * a.t * b.t
		this.c = -0.5 * (pos+neg)
	}
	def apply(x: Seq[Double]) = wx(Data(x, 0)) + c
}

object SVM {
	val range = -2.0 to 2.0 by 0.05
	def main() {
		val data1 = new ArrayBuffer[(Seq[Double], Int)]
		for(r <- 0.0 to 2 * math.Pi by math.Pi / 18) {
			data1 += (Seq(0.75 + 1.0 * math.cos(r), 0.75 + 1.0 * math.sin(r)) -> +1)
			data1 += (Seq(-1.0 + 0.6 * math.cos(r), -1.0 + 0.6 * math.sin(r)) -> -1)
		}
		val data2 = ArrayBuffer[(Seq[Double], Int)](data1 :_*)
		data2 += (Seq(0.75, 0.75) -> -1)
		data2 += (Seq(-1.0, -1.0) -> +1)
		val data3 = new ArrayBuffer[(Seq[Double], Int)]
		for(r <- 0.0 to 2 * math.Pi by math.Pi / 9) {
			val x1 = +1.2 + 0.4 * Math.cos(r)
			val y1 = +0.0 + 0.4 * Math.sin(r)
			val x2 = -1.2 + 0.4 * Math.cos(r)
			val y2 = +0.0 + 0.4 * Math.sin(r)
			data3 += (Seq(x1, y1) -> -1)
			data3 += (Seq(x2, y2) -> -1)
			val x3 = +0.0 + 0.4 * Math.cos(r)
			val y3 = +1.2 + 0.4 * Math.sin(r)
			val x4 = +0.0 + 0.4 * Math.cos(r)
			val y4 = -1.2 + 0.4 * Math.sin(r)
			data3 += (Seq(x3, y3) -> +1)
			data3 += (Seq(x4, y4) -> +1)
		}
		val data4 = new ArrayBuffer[(Seq[Double], Int)]
		for(r <- 0.0 to 2 * math.Pi by math.Pi / 18) {
			val x1 = 1.6 * math.cos(r)
			val y1 = 1.6 * math.sin(r)
			data4 += (Seq(x1, y1) -> -1)
			val x2 = 0.6 * math.cos(r)
			val y2 = 0.6 * math.sin(r)
			data4 += (Seq(x2, y2) -> +1)
		}
		test(1, data1, 1e300, (a, b) => (a, b).zipped.map(_*_).sum)
		test(2, data2, 1e-10, (a, b) => (a, b).zipped.map(_*_).sum)
		test(3, data3, 1e-10, (a, b) => math.exp(- 8 * (a, b).zipped.map(_-_).map(math.pow(_, 2)).sum))
		test(4, data4, 1e-10, (a, b) => math.exp(- 8 * (a, b).zipped.map(_-_).map(math.pow(_, 2)).sum))
	}
	def test(num: Int, data: Seq[(Seq[Double], Int)], C: Double, k: (Seq[Double],Seq[Double])=>Double) {
		val svm = new SVM(data, C, k)
		val out0 = new PrintStream("data0.dat"); for((x,t) <- data if t == +1) out0.println(x.mkString(",")); out0.close;
		val out1 = new PrintStream("data1.dat"); for((x,t) <- data if t == -1) out1.println(x.mkString(",")); out1.close;
		val out2 = new PrintStream("dense.dat")
		for(y <- range) out2.println(range.map(x => svm(Seq(x, y))).mkString(","))
		out2.close
		(s"python src/main/python/SVM.py $num" #&& "rm dense.dat data0.dat data1.dat" #&& s"open plot$num.svg" !)
	}
}
