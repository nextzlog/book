package apps

import java.io.PrintStream
import scala.language.postfixOps
import scala.sys.process._

trait Active {
	def fp(z: Seq[Double]): Seq[Double]
	def bp(y: Seq[Double]): Seq[Double]
}

class Sigmoid extends Active {
	def fp(z: Seq[Double]) = z.map(z=>1/(1+Math.exp(-z)))
	def bp(z: Seq[Double]) = fp(z).map(y=>y*(1-y))
}

trait Neural {
	val dim: Int
	def apply(x: Seq[Double]): Seq[Double]
	def apply(x: Seq[Double], e: Seq[Double]): Seq[Double]
}

class Output(val dim: Int = 1) extends Neural {
	def apply(x: Seq[Double]) = x
	def apply(x: Seq[Double], e: Seq[Double]) = e
}

class Neuron(val dim: Int, act: Active, next: Neural, sgd: ()=>SGD) extends Neural {
	val W = Seq.fill(next.dim, dim)(sgd())
	def apply(x: Seq[Double]) = next(act.fp(z(x)))
	def apply(x: Seq[Double], e: Seq[Double]): Seq[Double] = {
		val xE = next(act.fp(z(x)), e)
		val zE = (xE, act.bp(z(x))).zipped.map(_*_)
		for((w,ze)<-W zip zE; (w,x)<-w zip x) w.update(x*ze)
		return W.transpose.map((_,zE).zipped.map(_.w*_).sum)
	}
	def z(x: Seq[Double]) = W.map((_,x).zipped.map(_.w*_).sum)
}

class Offset(val dim: Int, act: Active, next: Neural, sgd: ()=>SGD) extends Neural {
	val hidden = new Neuron(dim + 1, act, next, sgd)
	def offset = hidden.W.map(_.last.w)
	def apply(x: Seq[Double]) = hidden(x:+1d)
	def apply(x: Seq[Double], e: Seq[Double]) = hidden(x:+1d, e).init
}

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

object MLP {
	val range = -1.0 to 2.0 by 0.005
	val data1 = for(x<-0 to 1;y<-0 to 1) yield Seq[Double](x,y)->(x|y)
	val data2 = for(x<-0 to 1;y<-0 to 1) yield Seq[Double](x,y)->(x^y)
	def main() {
		val neuron0 = new Offset(2, new Sigmoid, new Output(1), ()=>new PlainSGD)
		val neuron3 = new Output(1)
		val neuron2 = new Neuron(3, new Sigmoid, neuron3, ()=>new PlainSGD(0.1))
		val neuron1 = new Neuron(2, new Sigmoid, neuron2, ()=>new PlainSGD(0.1))
		val offset3 = new Output(1)
		val offset2 = new Offset(3, new Sigmoid, offset3, ()=>new PlainSGD(0.1))
		val offset1 = new Offset(2, new Sigmoid, offset2, ()=>new PlainSGD(0.1))
		test(neuron0, 0, data1)
		test(neuron1, 1, data2)
		test(offset1, 2, data2)
		val epoch = 900
		val w1 = Seq.fill(10, 3, 3)(math.random)
		val w2 = Seq.fill(10, 1, 4)(math.random)
		test("PlainSGD", ()=>new PlainSGD, epoch, w1, w2)
		test("AdaDelta", ()=>new AdaDelta, epoch, w1, w2)
		("python src/main/python/MLP.py GD1 PlainSGD AdaDelta" #&& "rm PlainSGD.dat AdaDelta.dat" #&& "open plot.svg" !)
		println(test("PlainSGD", ()=>new PlainSGD))
		println(test("AdaDelta", ()=>new AdaDelta))
		("python src/main/python/MLP.py GD2 PlainSGD AdaDelta" #&& "rm PlainSGD.dat AdaDelta.dat" #&& "open plot.svg" !)
	}
	def test(model: Neural, num: Int, data: Seq[(Seq[Double], Int)]) {
		for(n<-1 to 500000; (x,t)<-data) model(x, Seq(model(x).head-t))
		val out = new PrintStream("dist.dat")
		for(y <- range) out.println(range.map(x => model(Seq(x, y))(0)).mkString(","))
		out.close
		(s"python src/main/python/MLP.py MLP $num" #&& "rm dist.dat" #&& s"open plot$num.svg" !)
	}
	def test(name: String, sgd: ()=>SGD, epoch: Int, w1: Seq[Seq[Seq[Double]]], w2: Seq[Seq[Seq[Double]]]) {
		val loss = Array.ofDim[Double](epoch, w1.size)
		for(stage <- 0 until w1.size) {
			val model3 = new Output(1)
			val model2 = new Offset(3, new Sigmoid, model3, sgd)
			val model1 = new Offset(2, new Sigmoid, model2, sgd)
			for((w,w1)<-model1.hidden.W zip w1(stage); (w,w1)<-w zip w1) w.w = w1
			for((w,w2)<-model2.hidden.W zip w2(stage); (w,w2)<-w zip w2) w.w = w2
			for(n<-0 until epoch) {
				for((x,t)<-data2) model1(x, Seq(model1(x).head - t))
				loss(n)(stage) = data2.map{case (x,t)=>math.pow(model1(x).head - t, 2)}.sum / data2.size
			}
		}
		val out = new PrintStream(name + ".dat")
		for(l <- loss) out.println(l.mkString(","))
		out.close
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
