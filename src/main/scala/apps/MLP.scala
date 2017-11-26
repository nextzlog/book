package apps

import java.io.PrintStream

trait Active {
	def fp(z: Seq[Double]): Seq[Double]
	def bp(y: Seq[Double]): Seq[Double]
}

class Sigmoid extends Active {
	def fp(z: Seq[Double]) = z.map(z=>1/(1+Math.exp(-z)))
	def bp(z: Seq[Double]) = fp(z).map(y=>y*(1-y))
}

class Softmax extends Active {
	def fp(z: Seq[Double]) = z.map(math.exp(_)/z.map(math.exp).sum)
	def bp(z: Seq[Double]) = z.map(_=>1.0)
}

trait Neural {
	val dim: Int
	def apply(x: Seq[Double]): Seq[Double]
	def apply(x: Seq[Double], t: Seq[Double]): Seq[Double]
}

class Output(val dim: Int = 1, loss: (Double,Double)=>Double = _-_) extends Neural {
	def apply(x: Seq[Double]) = x
	def apply(x: Seq[Double], t: Seq[Double]) = (x,t).zipped.map(loss)
}

class Neuron(val dim: Int, act: Active, next: Neural, sgd: ()=>SGD) extends Neural {
	val W = Seq.fill(next.dim, dim)(sgd())
	def apply(x: Seq[Double]) = next(act.fp(z(x)))
	def apply(x: Seq[Double], t: Seq[Double]): Seq[Double] = {
		val xE = next(act.fp(z(x)), t)
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
	def apply(x: Seq[Double], t: Seq[Double]) = hidden(x:+1d, t).init
}

object MLP {
	val data1 = for(x<-0 to 1;y<-0 to 1) yield (x,y,x|y)
	val data2 = for(x<-0 to 1;y<-0 to 1) yield (x,y,x^y)
	val data3 = Seq((0,1,0), (1,0,1), (0,-1,2), (-1,0,3))
	def main() {
		val binary3 = new Output(1, _-_)
		val binary0 = new Offset(2, new Sigmoid, binary3, ()=>new PlainSGD)
		val binary2 = new Neuron(3, new Sigmoid, binary3, ()=>new PlainSGD)
		val binary1 = new Neuron(2, new Sigmoid, binary2, ()=>new PlainSGD)
		val offset2 = new Offset(3, new Sigmoid, binary3, ()=>new PlainSGD)
		val offset1 = new Offset(2, new Sigmoid, offset2, ()=>new PlainSGD)
		binary(binary0, 0, data1)
		binary(binary1, 1, data2)
		binary(offset1, 2, data2)
		val select3 = new Output(4, _-_)
		val select2 = new Offset(3, new Softmax, select3, ()=>new PlainSGD)
		val select1 = new Offset(2, new Sigmoid, select2, ()=>new PlainSGD)
		select(select1, 3, data3, data3.size)
	}
	def binary(model: Neural, num: Int, data: Seq[(Int,Int,Int)]) {
		val range = -1.0 to 2.0 by 0.005
		for(n<-1 to 200000; (x,y,t)<-data) model(Seq(x,y), Seq(t))
		val out = new PrintStream("dist.dat")
		for(y <- range) out.println(range.map(x => model(Seq(x, y)).head).mkString(","))
		out.close
		exec.Python.run("MLP", num)
	}
	def select(model: Neural, num: Int, data: Seq[(Int,Int,Int)], D: Int) {
		val range = -2.0 to 2.0 by 0.005
		for(n<-1 to 100000; (x,y,t)<-data) model(Seq(x,y), Seq.tabulate(D)(d=>if(t==d) 1 else 0))
		val out = new PrintStream("dist.dat")
		for(y <- range) out.println(range.map(x => model(Seq(x, y)).zipWithIndex.maxBy(_._1)._2).mkString(","))
		out.close
		exec.Python.run("MLP", num)
	}
}
