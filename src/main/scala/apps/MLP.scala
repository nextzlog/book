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

object MLP {
	val range = -1.0 to 2.0 by 0.005
	val data1 = for(x<-0 to 1;y<-0 to 1) yield Seq[Double](x,y)->(x|y)
	val data2 = for(x<-0 to 1;y<-0 to 1) yield Seq[Double](x,y)->(x^y)
	def main() {
		{
			val model3 = new Output(1)
			val model2 = new Offset(3, new Sigmoid, model3, ()=>new PlainSGD)
			val model1 = new Offset(2, new Sigmoid, model2, ()=>new PlainSGD)
			val samples = for(x<-0 to 1; y<-0 to 1) yield Seq[Double](x,y)->(x^y)
			for(n<-1 to 1000000; (x,t)<-samples) model1(x, Seq(model1(x).head-t))
		}
		{
			val neuron = new Offset(2, new Sigmoid, new Output, ()=>new PlainSGD)
			val samples = for(x<-0 to 1; y<-0 to 1) yield Seq[Double](x,y)->(x|y)
			for(n<-1 to 1000000; (x,t)<-samples) neuron(x, Seq(neuron(x).head-t))
		}

		
		
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
	}
	def test(model: Neural, num: Int, data: Seq[(Seq[Double], Int)]) {
		for(n<-1 to 500000; (x,t)<-data) model(x, Seq(model(x).head-t))
		val out = new PrintStream("dist.dat")
		for(y <- range) out.println(range.map(x => model(Seq(x, y))(0)).mkString(","))
		out.close
		(s"python src/main/python/MLP.py $num" #&& "rm dist.dat" #&& s"open plot$num.svg" !)
	}
}
