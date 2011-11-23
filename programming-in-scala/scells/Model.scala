package org.stairwaybook.scells

class Model(val height: Int,  val width: Int) {
  case class Cell(row: Int,  column: Int) {
    var formula: Formula = Empty
    override def toString = formula.toString
  }

  val cells = Array.ofDim[Cell](height, width)

  for (i <- 0 until height; j <- 0 until width)
    cells(i)(j) = new Cell(i, j)
}