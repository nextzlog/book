import apps._

object Main {
	def main(args: Array[String]) = args.head match {
		case "DT"  => DT.main()
		case "GMM" => GMM.main()
		case "KNN" => KNN.main()
		case "LDA" => LDA.main()
		case "LR"  => LR.main()
		case "MLP" => MLP.main()
		case "NBC" => NBC.main()
		case "SGD" => SGD.main()
		case "SVM" => SVM.main()
	}
}
