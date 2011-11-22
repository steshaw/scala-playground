import swing._
import event.EditDone

object TempConverter extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "Celcius/Fahrenheit Converter"
    object celcius extends TextField { columns = 5 }
    object farenheight extends TextField { columns = 5 }
    contents = new FlowPanel {
      contents += celcius
      contents += new Label(" Celsius  =  ")
      contents += farenheight
      contents += new Label(" Farenheight")
      border = Swing.EmptyBorder(15, 10, 10, 10)
      listenTo(celcius, farenheight)
      reactions += {
        case EditDone(`farenheight`) =>
          val f = farenheight.text.toInt
          val c = (f - 32) * 5 / 9
          celcius.text = c.toString
        case EditDone(`celcius`) =>
          val c = celcius.text.toInt
          val f = c * 9 / 5 + 32
          farenheight.text = f.toString
      }
    }
  }
}