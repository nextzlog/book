package wire

import javafx.animation.{Animation, KeyFrame, Timeline}
import javafx.application.Application
import javafx.event.{ActionEvent, EventHandler}
import javafx.scene.Scene
import javafx.scene.canvas.Canvas
import javafx.scene.control.ToggleButton
import javafx.scene.layout.VBox
import javafx.scene.paint.Color
import javafx.stage.Stage
import javafx.util.Duration

class CA2[S](rule: Seq[Seq[S]] => S, d: Int = 1) {
	def ROI[V](i: Int)(s: Seq[V]) = Range.inclusive(i - d, i + d).map(Math.floorMod(_, s.size)).map(s)
	def apply(s: Seq[Seq[S]]) = s.indices.map(x => s(x).indices.map(y => rule(ROI(x)(s).map(ROI(y)))))
}

object WireWorldRule extends CA2[Char](ROI => ROI(1)(1) match {
	case 'W' if(ROI.flatten.count(_ == 'H') == 1) => 'H'
	case 'W' if(ROI.flatten.count(_ == 'H') == 2) => 'H'
	case 'W' => 'W'
	case 'H' => 'T'
	case 'T' => 'W'
	case 'B' => 'B'
})

class WireWorldView(w: Int, h: Int, u: Int = 16, g: Int = 1) extends Canvas(u * w, u * h) {
	var data = Array.fill(w, h)('B')
	val gc2d = getGraphicsContext2D
	def draw = for(x <- 0 until w; y <- 0 until h) {
		gc2d.setFill(data(x)(y) match {
			case 'B' => Color.rgb(0x00, 0x00, 0x00)
			case 'W' => Color.rgb(0xff, 0xff, 0x00)
			case 'H' => Color.rgb(0xff, 0x00, 0x00)
			case 'T' => Color.rgb(0x00, 0x00, 0xff)
		})
		gc2d.fillRect(u * x, u * y, u - g, u - g)
	}
	def next = data = WireWorldRule(data.map(_.toSeq).toSeq).map(_.toArray).toArray
}

class AnimeWireWorldView(w: Int, h: Int, u: Int = 16, g: Int = 1) extends WireWorldView(w, h, u, g) {
	setOnMouseClicked(e => {
		val (x, y) = (e.getX / u).toInt -> (e.getY / u).toInt
		data(x)(y) = data(x)(y) match {
			case 'B' => 'W'
			case 'W' => 'H'
			case 'H' => 'T'
			case 'T' => 'B'
		}
		draw
	})
	val line = new Timeline(new KeyFrame(new Duration(500), new EventHandler[ActionEvent] {
		override def handle(e: ActionEvent) = {next; draw;}
	}))
	line.setCycleCount(Animation.INDEFINITE)
}

class MainView extends Application {
	override def start(stage: Stage) = {
		val grid = new AnimeWireWorldView(80, 33)
		val button = new ToggleButton("START/PAUSE") {
			setOnAction(e => if(isSelected) grid.line.play else grid.line.stop)
		}
		stage.setScene(new Scene(new VBox(grid, button)))
		stage.setTitle("WireWorld")
		stage.show
		grid.draw
	}
}

object Main {
	def main(args: Array[String]) = Application.launch(classOf[MainView], args: _*)
}
