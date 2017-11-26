package apps

import java.io.PrintStream

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
		this.eE = r*eE + (1-r) * math.pow(1*E, 2)
		val n = math.sqrt(eW+e) / math.sqrt(eE+e)
		this.eW = r*eW + (1-r) * math.pow(n*E, 2)
		this.w -= n * E
	}
}

object SGD {
	def main() {
		println(test("PlainSGD", ()=>new PlainSGD))
		println(test("AdaDelta", ()=>new AdaDelta))
		exec.Python.run("SGD", 1, "PlainSGD", "AdaDelta")
		val epoch = 1e5.toInt
		val w1 = Seq.fill(5, 3, 3)(math.random)
		val w2 = Seq.fill(5, 1, 4)(math.random)
		test("PlainSGD", ()=>new PlainSGD, epoch, w1, w2)
		test("AdaDelta", ()=>new AdaDelta, epoch, w1, w2)
		exec.Python.run("SGD", 2, "PlainSGD", "AdaDelta")
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
			x.update(+x.w * 2)
			y.update(-y.w * 2)
		} while(x.w.abs < 2 && y.w.abs < 2)
		out.println("%f,%f".format(x.w, y.w))
		out.close
		cnt
	}
	def test(name: String, sgd: ()=>SGD, epoch: Int, w1: Seq[Seq[Seq[Double]]], w2: Seq[Seq[Seq[Double]]]) {
		val shot = 1.to(9) ++ 0.to(math.log10(epoch).toInt-2).map(e=>10.to(99).map(_*math.pow(10,e).toInt)).flatten :+ epoch
		val plot = Array.ofDim[Double](shot.size, w1.size)
		val data = for(x<-0 to 1;y<-0 to 1) yield Seq[Double](x,y)->(x^y)
		for(stage <- 0 until w1.size) {
			val model3 = new Output(1, _-_)
			val model2 = new Offset(3, new Sigmoid, model3, sgd)
			val model1 = new Offset(2, new Sigmoid, model2, sgd)
			for((w,w1)<-model1.hidden.W zip w1(stage); (w,w1)<-w zip w1) w.w = w1
			for((w,w2)<-model2.hidden.W zip w2(stage); (w,w2)<-w zip w2) w.w = w2
			for(n<-1 to epoch) {
				for((x,t)<-data) model1(x, Seq(t))
				if(shot.contains(n)) plot(shot.indexOf(n))(stage) = data.map{case (x,t)=>math.pow(model1(x).head - t, 2)}.sum / data.size
			}
		}
		val out = new PrintStream(name + ".dat")
		for((epoch, loss) <- (shot, plot).zipped) out.println("%d,%s".format(epoch, loss.mkString(",")))
		out.close
	}
}
