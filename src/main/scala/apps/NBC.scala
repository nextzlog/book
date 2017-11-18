package apps

import java.io.PrintStream
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

class NaiveBayes[W,C](docs: Seq[Seq[W]], cats: Seq[C]) {
	val V = docs.flatten.toSet
	val P = cats.groupBy(c=>c).map{case(c,s)=>c->s.size.toDouble/docs.size}
	import scala.collection.mutable.Map
	val N = Map[(W,C),Double]().withDefaultValue(0)
	for((d,c)<-(docs,cats).zipped;w<-d) N(w,c) += 1
	def Pwc(w: W, c: C) = (N(w,c)+1) / V.map(N(_,c)+1).sum
	def Pcd(c: C, d: Seq[W]) = math.log(P(c)) + d.map(w=>math.log(Pwc(w,c))).sum
	def apply(d: Seq[W]) = cats.distinct.maxBy(Pcd(_, d))
}

object NBC {
	def main() {
		val prefs = for(p<-1 to 47) yield {
			val src = Source.fromResource("p%02d".format(p))
			val words = src.getLines.toList
			src.close
			words.head -> words.tail
		}
		val area2 = for(r <-1 to 2) yield {
			val src = Source.fromResource("r2%d".format(r))
			val words = src.getLines.toList.tail
			src.close
			words.head -> words.tail
		}
		val area8 = for(r <-1 to 8) yield {
			val src = Source.fromResource("r8%d".format(r))
			val words = src.getLines.toList.tail
			src.close
			words.head -> words.tail
		}
		val nb2 = new NaiveBayes(area2.map(_._2), area2.map(_._1)) {
			val out = new PrintStream("pref.dat")
			for((pref,words) <- prefs) out.println("%s,%s".format(pref, this(words)))
			out.close
			"python2 src/main/python/NBC.py 2" #&& "rm pref.dat" !
		}
		val nb8 = new NaiveBayes(area8.map(_._2), area8.map(_._1)) {
			val out = new PrintStream("pref.dat")
			for((pref,words) <- prefs) out.println("%s,%s".format(pref, this(words)))
			out.close
			"python2 src/main/python/NBC.py 8" #&& "rm pref.dat" !
		}
	}
}
