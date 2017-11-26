package apps

import java.io.PrintStream
import scala.io.Source

class LDA[W,D](docs: Map[D, Seq[W]], K: Int, a: Double = 0.1, b: Double = 0.01) {
	case class Word(v: W, var z: Int = util.Random.nextInt(K))
	val W = docs.mapValues(_.map(Word(_)).toArray)
	val V = W.flatMap(_._2).groupBy(_.v)
	def Ndk(d: D) = 0.until(K).map(k=>W(d).count(_.z==k) + a)
	def Nkv(v: W) = 0.until(K).map(k=>V(v).count(_.z==k) + b)
	val NkV = V.keys.map(Nkv).reduce((_,_).zipped.map(_+_)).toArray
	for(step<-1 to 1000; d<-docs.keys; (w,n)<-docs(d).zipWithIndex) {
		NkV(W(d)(n).z) -= 1
		W(d)(n).z = -1
		val S = (Nkv(w), Ndk(d), NkV).zipped.map(_*_/_).scan(.0)(_+_)
		val r = util.Random.nextDouble * S.last
		W(d)(n).z = S.tail.indexWhere(_ >= r)
		NkV(W(d)(n).z) += 1
	}
	def apply(d: D) = 0.until(K).maxBy(Ndk(d)(_))
}

object LDA {
	def main() {
		val prefs = for(p<-1 to 47) yield {
			val src = Source.fromResource("p%02d".format(p))
			val words = src.getLines.toList
			src.close
			words.head -> words.tail
		}
		for(k <- Seq(3, 5).par) new LDA(prefs.toMap, k) {
			val out = new PrintStream(s"pref$k.dat")
			for((pref,words) <- prefs) out.println("%s,%s".format(pref, this(pref)))
			out.close
			exec.Python.run("LDA", k)
		}
	}
}
