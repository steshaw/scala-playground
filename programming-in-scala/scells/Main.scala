package org.stairwaybook.scells

import swing.{MainFrame, SimpleSwingApplication}

object Main extends SimpleSwingApplication {
  def top = new MainFrame {
    title = "ScalaSheet"
    contents = new Spreadsheet(100, 28)
  }
}