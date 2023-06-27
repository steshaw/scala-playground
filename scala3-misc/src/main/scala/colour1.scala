object Colour1:
  def main(args: Array[String]) =
    enum Colour:
      case Red, Green, Blue

    import Colour.*
    for (c <- Colour.values)
      println(c)
