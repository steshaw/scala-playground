import swing._
import event.ButtonClicked

object SecondSwing extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Second Scala Swing App"
    val button = new Button {
      text = "Click me"
    }
    val label = new Label {
      text = "No button clicks registered"
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += button
      contents += label
      border = Swing.EmptyBorder(30, 30, 10, 30)
    }
    var numClicks = 0
    reactions += {
      case ButtonClicked(b) =>
      numClicks += 1
      label.text = "Number of clicks: %s".format(numClicks)
    }
    listenTo(button)
  }
}
