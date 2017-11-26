package apps

import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer

class KNN[D,T](k: Int, data: Seq[(D,T)], d: (D,D)=>Double) {
	def apply(x: D) = data.sortBy(s=>d(x,s._1)).take(k).groupBy(_._2).maxBy(_._2.size)._1
}

object KNN {
	val range = -10.0 to 10.0 by 0.1
	def quad(a: Seq[Double], b: Seq[Double]) = (a, b).zipped.map(_-_).map(x => x * x).sum
	def main() {
		val size = 300
		val random = new util.Random()
		val data = new ArrayBuffer[(Seq[Double], Int)]
		val M = Seq(Seq(+60, +60), Seq(+20, -20))
		val S = Seq(Seq(+16, +16), Seq(+20, +20))
		val W = Seq(0.4, 0.6)
		for (k <- 0 until W.size; i <- 1 to (W(k) * size).toInt) {
			val x = 0.1 * Math.round((random.nextGaussian * S(k)(0) + M(k)(0))).toDouble
			val y = 0.1 * Math.round((random.nextGaussian * S(k)(1) + M(k)(1))).toDouble
			if (range.contains(x) && range.contains(y)) {
				data += ((Seq(+x, +y), +k))
				data += ((Seq(-x, -y), -k))
			}
		}
		val knn = new KNN(5, data, quad _)
		util.Try {
			val out = new PrintStream("data0.dat")
			for((d, k) <- data if k == -1) out.println(d.mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("data1.dat")
			for((d, k) <- data if k ==  0) out.println(d.mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("data2.dat")
			for((d, k) <- data if k == +1) out.println(d.mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("class.dat")
			for(y <- range) out.println(range.map(x => knn(Seq(x, y))).mkString(","))
			out.close
		}
		exec.Python.run("KNN")
	}
}
