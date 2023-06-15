enum Colour:
  case Red, Green, Blue

import Colour.*

def main(args: Array[String]) =
  for (c <- Colour.values)
    println(c)
