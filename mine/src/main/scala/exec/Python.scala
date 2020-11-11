package exec

import java.io.{File,PrintStream}
import scala.io.Source
import scala.language.postfixOps
import scala.sys.process._

object Python {
	def run(name: String, args: Any*) {
		val dir = System.getProperty("java.io.tmpdir")
		val tmp = File.createTempFile(dir, "%s.py".format(name))
		tmp.deleteOnExit
		val src = Source.fromResource("%s.py".format(name))
		val out = new PrintStream(tmp)
		src.getLines.foreach(out.println)
		src.close
		out.close
		"python3 %s %s".format(tmp.getPath, args.mkString(" ")) !
	}
}
