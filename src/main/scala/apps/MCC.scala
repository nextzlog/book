package apps

import java.io.PrintStream
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class Softmax extends Active {
	def fp(z: Seq[Double]) = z.map(math.exp(_)/z.map(math.exp).sum)
	def bp(z: Seq[Double]) = z.map(_=>1.0)
}

object MCC {
	def main() {
		val model3 = new Output(3, _-_)
		val model2 = new Offset(30, new Sigmoid, model3, ()=>new PlainSGD)
		val model1 = new Offset(2, new Sigmoid, model2, ()=>new PlainSGD)
		val src = Source.fromResource("map")
		val map2 = src.getLines.map(_.split(",").map(_.toInt).toList).toList
		val map = 0.until(map2.size).by(20).map(y=>0.until(map2(y).size).by(20).map(x=>map2(y)(x)))
		src.close
		val H = map.size
		val W = map.head.size
		for(y<-0 until H) println(0.until(W).map(x=>map(y)(x)).mkString(" "))
		val total = 100000
		for(n<-1 to total) {
			val start = System.nanoTime()
			for(y<-0 until H; x<-0 until W) model1(Seq(x,y), Seq.tabulate(3)(i=>if(i==map(y)(x)) 1 else 0))
			println(1e-9 * (System.nanoTime() - start) * (total - n) / 60 / 60 + " hours")
		}
		val out = new PrintStream("dist.dat")
		for(y<-0 until H) out.println(0.until(W).map(x => model1(Seq(x,y)).zipWithIndex.maxBy(_._1)._2).mkString(","))
		out.close
		(s"python src/main/python/MCC.py" #&& s"open plot.svg" !)
	}
}
