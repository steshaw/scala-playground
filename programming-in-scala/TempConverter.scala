import swing._
import event.EditDone

object TempConverter extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Celcius/Fahrenheit Converter"
    object celcius extends TextField { columns = 5 }
    object farenheit extends TextField { columns = 5 }
    contents = new FlowPanel {
      contents += celcius
      contents += new Label(" Celsius  =  ")
      contents += farenheit
      contents += new Label(" Farenheight")
      border = Swing.EmptyBorder(15, 10, 10, 10)
      listenTo(celcius, farenheit)
      reactions += {
        case EditDone(`farenheit`) =>
          val f = farenheit.text.toInt
          val c = (f - 32) * 5 / 9
          celcius.text = c.toString
        case EditDone(`celcius`) =>
          val c = celcius.text.toInt
          val f = c * 9 / 5 + 32
          farenheit.text = f.toString
      }
    }
  }
}