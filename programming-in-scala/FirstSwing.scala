import scala.swing._

object FirstSwing extends SimpleGUIApplication {
  def top = new MainFrame {
    title = "First Scala Swing App"
    contents = new Button {
      text = "Click me"
    }
  }
}
