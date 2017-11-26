package apps

import java.io.PrintStream
import scala.collection.mutable.ArrayBuffer

trait Node[T] {
	def apply(x: Seq[Int]): T
}

case class Question[T](Y: Seq[(Seq[Int], T)]) extends Node[T] {
	lazy val freqs = Y.groupBy(_._2).map(_._2.size.toDouble / Y.size)
	lazy val ent = freqs.map(f => -f * math.log(f)).sum / math.log(2)
	lazy val major = Y.groupBy(_._2).maxBy(_._2.size)._1
	lazy val v = Y.head._1.indices.map(Variable(Y, _)).minBy(_.t.ent)
	def apply(x: Seq[Int]) = if(ent - v.t.ent < 1e-5) major else v(x)
}

case class Variable[T](Y: Seq[(Seq[Int], T)], axis: Int) extends Node[T] {
	val t = Y.map(_._1(axis)).distinct.map(Division(Y, axis, _)).minBy(_.ent)
	def apply(x: Seq[Int]) = t(x)
}

case class Division[T](Y: Seq[(Seq[Int], T)], axis: Int, value: Int) extends Node[T] {
	val sn1 = Question(Y.filter(_._1(axis) >  value))
	val sn2 = Question(Y.filter(_._1(axis) <= value))
	val ent = (sn1.ent * sn1.Y.size + sn2.ent * sn2.Y.size) / Y.size
	def apply(x: Seq[Int]) = if(x(axis) >= value) sn1(x) else sn2(x)
}

case class Bagging[T](Y: Seq[(Seq[Int], T)], T: Int, N: Int) extends Node[T] {
	val t = Seq.fill(T)(Question(Seq.fill(N)(Y(util.Random.nextInt(Y.size)))))
	def apply(x: Seq[Int]) = t.map(_(x)).groupBy(identity).maxBy(_._2.size)._1
}

case class Resample[T](Y: Seq[(Seq[Int], T)], P: Seq[Double]) extends Node[T] {
	def reject(i: Int) = if(util.Random.nextDouble * P.max < P(i)) Y(i) else null
	val data = new collection.mutable.ArrayBuffer[(Seq[Int], T)]
	while(data.size < P.size) data += reject(util.Random.nextInt(P.size)) -= null
	val quest = Question(data)
	val error = Y.zip(P).map{case ((x, y), p) => if(quest(x) != y) p else 0}.sum
	def apply(x: Seq[Int]) = quest(x)
}

case class AdaStage[T](Y: Seq[(Seq[Int], T)], P: Seq[Double], M: Int) extends Node[T] {
	val best = List.fill(M)(Resample(Y, P.map(_ / P.sum))).minBy(_.error)
	val W = math.log((1 / best.error - 1) * (Y.map(_._2).toSet.size - 1))
	def isOK = best.error < 0.5
	def apply(x: Seq[Int]) = best(x)
	def apply(x: Seq[Int], y: T): Double = if(best(x) == y) W else 0
	val next = Y.zip(P).map{case ((x, y), p) => p * math.exp(W - this(x, y))}
}

case class AdaBoost[T](Y: Seq[(Seq[Int], T)], M: Int) extends Node[T] {
	val stages = Seq(AdaStage(Y, Y.map(_ => 1.0 / Y.size), M)).toBuffer
	while(stages.last.isOK) stages += AdaStage(Y, stages.last.next, M)
	def apply(x: Seq[Int], y: T): Double = stages.init.map(_(x, y)).sum
	def apply(x: Seq[Int]) = Y.map(_._2).distinct.maxBy(this(x, _))
}

object DT {
	val range = -100 to 100
	def main() {
		val size = 300
		val random = new util.Random()
		val data = new ArrayBuffer[(Seq[Int], Int)]
		val M = Seq(Seq(+60, +60), Seq(+20, -20))
		val S = Seq(Seq(+16, +16), Seq(+20, +20))
		val W = Seq(0.4, 0.6)
		for (k <- 0 until W.size; i <- 1 to (W(k) * size).toInt) {
			val x = (random.nextGaussian * S(k)(0) + M(k)(0)).toInt
			val y = (random.nextGaussian * S(k)(1) + M(k)(1)).toInt
			if (range.contains(x) && range.contains(y)) {
				data += ((Seq(+x, +y), +k))
				data += ((Seq(-x, -y), -k))
			}
		}
		val K = 50
		test(s"plain", Question(data), data)
		test(s"bag$K", Bagging (data, K, size / 5), data)
		test(s"ada$K", AdaBoost(data, K), data)
	}
	def test(id: String, root: Node[Int], data: Seq[(Seq[Int], Int)]) {
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
			for(y <- range) out.println(range.map(x => root(Seq(x, y))).mkString(","))
			out.close
		}
		exec.Python.run("DT", id)
	}
}
