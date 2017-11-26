package apps

import java.io._

class Kmeans(X: Seq[Seq[Double]], K: Int, d: (Seq[Double],Seq[Double])=>Double) {
	val M = Array.fill(K, X.map(_.size).min)(Math.random)
	def quad(a: Seq[Double], b: Seq[Double]) = (a,b).zipped.map(_-_).map(d=>d*d).sum
	def apply(x: Seq[Double]) = M.map(d(_,x)).zipWithIndex.minBy(_._1)._2
	def estep = X.groupBy(apply).values.map(c=>c.transpose.map(_.sum / c.size))
	for(step<-1 to 100) (estep, M).zipped.foreach(_.copyToArray(_))
}

class GMM(val D: Int, val K: Int) {
	val W = Array.fill(K)(1.0 / K)
	val M = Array.fill(K, D)(math.random)
	val S = Array.fill(K, D)(math.random)
	def apply(x: Seq[Double]) = for((w,m,s)<-(W,M,S).zipped) yield {
		val p = math.exp(-(x,m,s).zipped.map((x,m,s)=>(x-m)*(x-m)/s).sum/2)
		w * p / (math.pow(2 * math.Pi, .5 * m.size) * math.sqrt(s.product))
	}
}

class EM(X: Seq[Seq[Double]], K: Int) {
	val gmm = new GMM(X.map(_.size).min, K)
	def apply(x: Seq[Double]) = gmm(x).toSeq.zipWithIndex.maxBy(_._1)._2
	def mstep(P: Seq[Seq[Double]]): Double = {
		for(k<-0 until K) gmm.W(k) = P(k).sum / X.size
		val m1 = P.map((_,X).zipped.map((p,x)=>x.map(x=>p*x*1)).transpose)
		val m2 = P.map((_,X).zipped.map((p,x)=>x.map(x=>p*x*x)).transpose)
		for(k<-0 until K; d<-0 until gmm.D) {
			gmm.M(k)(d) = m1(k)(d).sum / P(k).sum
			gmm.S(k)(d) = m2(k)(d).sum / P(k).sum - math.pow(gmm.M(k)(d), 2)
		}
		X.map(x=>math.log(gmm(x).sum)).sum
	}
	for(step <- 1 to 100) println(mstep(X.map(gmm(_)).map(p=>p.map(_/p.sum)).transpose))
}

object GMM {
	def quad(a: Seq[Double], b: Seq[Double]) = (a,b).zipped.map(_-_).map(d=> d * d).sum
	def main() {
		val size = 2000
		val random = new util.Random()
		val data = Array.ofDim[Double](size, 2)
		val M = Array(Seq(-1.0, -1.0), Seq(+1.0, +1.0))
		val S = Array(Seq(+0.6, +0.6), Seq(+0.8, +0.8))
		val W = Array(+0.4, +0.6)
		val K = W.size
		val seg = new Array[Int](K + 1)
		for(i <- 1 to K) seg(i) = (W(i-1) * size).toInt
		for(i <- 1 to K) seg(i) = seg(i-1) + seg(i)
		seg(K) = size
		for (k <- 0 until K; i <- seg(k) until seg(k+1); d <- 0 until 2) data(i)(d) = random.nextGaussian * math.sqrt(S(k)(d)) + M(k)(d)
		val km = new Kmeans(data.map(_.toSeq).toSeq, K, quad _)
		util.Try {
			val out = new PrintStream("cents.dat");
			for(k <- 0 until K) out.println(km.M(k).mkString(","));
			out.close
		}
		for(k <- 0 until K) util.Try {
			val out = new PrintStream(s"mixt$k.dat");
			for(d <- data if km(d) == k) out.println(d.mkString(","));
			out.close
		}
		exec.Python.run("GMM", "KM")
		val em = new EM(data.map(_.toSeq).toSeq, K)
		util.Try {
			val out = new PrintStream("train.dat");
			for(s <- data) out.println(s(0) + "," + s(1));
			out.close
		}
		util.Try {
			val out = new PrintStream("dense.dat");
			for(y <- -3.0 to 3.0 by 0.05) out.println(-3.0.to(3.0).by(0.05).map(x => em.gmm(Seq(x,y)).sum).mkString(","))
			out.close
		}
		util.Try {
			val out = new PrintStream("cents.dat")
			for(k <- 0 until K) out.println(em.gmm.M(k).mkString(","))
			out.close
		}
		for(k <- 0 until K) util.Try {
			val out = new PrintStream(s"mixt$k.dat")
			for(d <- data if em(d) == k) out.println(d.mkString(","))
			out.close
		}
		exec.Python.run("GMM", "EM", if(em(data.head) == km(data.head)) "approve" else "reverse")
	}
}
