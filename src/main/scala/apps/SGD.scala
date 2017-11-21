package apps

import java.io.PrintStream
import scala.language.postfixOps
import scala.sys.process._

abstract class SGD(var w: Double = math.random) {
	def update(e: Double): Unit
}

class PlainSGD(e: Double = 0.01) extends SGD {
	def update(e: Double) = this.w -= e * this.e
}

class AdaDelta(r: Double = 0.95, e: Double = 1e-8) extends SGD {
	var eW = 0.0
	var eE = 0.0
	def update(E: Double) = {
		val n = math.sqrt(eW+e) / math.sqrt(eE+e)
		this.eW = r*eW + (1-r) * math.pow(n*E, 2)
		this.eE = r*eE + (1-r) * math.pow(1*E, 2)
		this.w -= n * E
	}
}

object SGD {
	def main() {
		println(test("PlainSGD", ()=>new PlainSGD))
		println(test("AdaDelta", ()=>new AdaDelta))
		("python src/main/python/SGD.py 1 PlainSGD AdaDelta" #&& "rm PlainSGD.dat AdaDelta.dat" #&& "open plot1.svg" !)
		val epoch = 900
		val w1 = Seq.fill(10, 3, 3)(math.random)
		val w2 = Seq.fill(10, 1, 4)(math.random)
		test("PlainSGD", ()=>new PlainSGD, epoch, w1, w2)
		test("AdaDelta", ()=>new AdaDelta, epoch, w1, w2)
		("python src/main/python/SGD.py 2 PlainSGD AdaDelta" #&& "rm PlainSGD.dat AdaDelta.dat" #&& "open plot2.svg" !)
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
	def test(name: String, sgd: ()=>SGD, epoch: Int, w1: Seq[Seq[Seq[Double]]], w2: Seq[Seq[Seq[Double]]]) {
		val loss = Array.ofDim[Double](epoch, w1.size)
		val data = for(x<-0 to 1;y<-0 to 1) yield Seq[Double](x,y)->(x^y)
		for(stage <- 0 until w1.size) {
			val model3 = new Output(1, _-_)
			val model2 = new Offset(3, new Sigmoid, model3, sgd)
			val model1 = new Offset(2, new Sigmoid, model2, sgd)
			for((w,w1)<-model1.hidden.W zip w1(stage); (w,w1)<-w zip w1) w.w = w1
			for((w,w2)<-model2.hidden.W zip w2(stage); (w,w2)<-w zip w2) w.w = w2
			for(n<-0 until epoch) {
				for((x,t)<-data) model1(x, Seq(t))
				loss(n)(stage) = data.map{case (x,t)=>math.pow(model1(x).head - t, 2)}.sum / data.size
			}
		}
		val out = new PrintStream(name + ".dat")
		for(l <- loss) out.println(l.mkString(","))
		out.close
	}
}
