enum Color:
  case Red, Green, Blue

import Color.*

def main(args: Array[String]) =
  for (c <- Color.values)
    println(c)
