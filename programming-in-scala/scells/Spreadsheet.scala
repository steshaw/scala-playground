package org.stairwaybook.scells

import java.awt.Color
import swing.{ListView, Table, ScrollPane}


class Spreadsheet(val height: Int, val width: Int) extends ScrollPane {
  val table = new Table(height, width) {
    rowHeight = 25
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new Color(150, 150, 150)
  }
  val rowHeader = new ListView((0 until  height) map (_.toString)) {
    fixedCellWidth = 30
    fixedCellHeight = table.rowHeight
  }
  viewportView = table
  rowHeaderView = rowHeader
}