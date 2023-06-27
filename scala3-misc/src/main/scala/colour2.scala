object Colour2:
  def main(args: Array[String]) =
    enum Colour(val rgb: Int):
      case Red   extends Colour(0xff0000)
      case Green extends Colour(0x00ff00)
      case Blue  extends Colour(0x0000ff)

    import Colour.*

    for (c <- Colour.values)
      printf(s"%10s = %06x\n", c, c.rgb)
