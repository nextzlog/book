package apps

import java.io.PrintStream
import scala.language.postfixOps
import scala.sys.process._

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

object GD1 {
	val data = for(x<-0 to 1;y<-0 to 1) yield Seq[Double](x,y)->(x^y)
	val trial = 10
	val epoch = 900
	val w1 = Seq.fill(trial, 3, 3)(math.random)
	val w2 = Seq.fill(trial, 1, 4)(math.random)
	def main() {
		test("PlainSGD", ()=>new PlainSGD, trial, epoch)
		test("AdaDelta", ()=>new AdaDelta, trial, epoch)
		"python src/main/python/GD1.py PlainSGD AdaDelta" #&& "rm PlainSGD.dat AdaDelta.dat" #&& "open plot.svg" !
	}
	def test(name: String, sgd: ()=>SGD, trial: Int, epoch: Int) {
		val loss = Array.ofDim[Double](epoch, trial)
		for(stage <- 0 until trial) {
			val model3 = new Output(1)
			val model2 = new Offset(3, new Sigmoid, model3, sgd)
			val model1 = new Offset(2, new Sigmoid, model2, sgd)
			for((w,w1)<-model1.hidden.W zip w1(stage); (w,w1)<-w zip w1) w.w = w1
			for((w,w2)<-model2.hidden.W zip w2(stage); (w,w2)<-w zip w2) w.w = w2
			for(n<-0 until epoch) {
				for((x,t)<-data) model1(x, Seq(model1(x).head - t))
				loss(n)(stage) = data.map{case (x,t)=>math.pow(model1(x).head - t, 2)}.sum / data.size
			}
		}
		val out = new PrintStream(name + ".dat")
		for(l <- loss) out.println(l.mkString(","))
		out.close
	}
}
