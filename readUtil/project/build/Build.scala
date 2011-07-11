import sbt._

class ReadUtil(info: ProjectInfo) extends DefaultProject(info) {
  /*
  name := "ReadUntil"

  version := "1.0"

  scalaVersion := "2.8.1"
  */

  val jline = "jline" % "jline" % "0.9.92"

  override def mainClass = Some("ReadUtil")

//  libraryDependencies += "jline" % "jline" % "jline"
}
