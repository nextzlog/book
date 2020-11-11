import apps._

object Main {
	def main(args: Array[String]): Unit = {
		args.headOption match {
			case Some("DT" ) => DT.main()
			case Some("GMM") => GMM.main()
			case Some("KNN") => KNN.main()
			case Some("LDA") => LDA.main()
			case Some("LR" ) => LR.main()
			case Some("MLP") => MLP.main()
			case Some("NBC") => NBC.main()
			case Some("SGD") => SGD.main()
			case Some("SVM") => SVM.main()
			case _ => println("usage: java -jar mine.jar [app]")
		}
	}
}
