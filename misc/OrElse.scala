object OrElse extends App {

  def scalazVersion1(buildScalaVersion: String) = {
    lazy val scalazVersion = Map("2.8.0" -> "5.0", "2.8.1" -> "5.0").getOrElse(buildScalaVersion, "6.0.2")
    scalazVersion
  }
  def scalazVersion2(buildScalaVersion: String) = {
    lazy val scalazVersion = Map("2.8.0" -> "5.0", "2.8.1" -> "5.0") get buildScalaVersion getOrElse "6.0.2"
    scalazVersion
  }

  Console println scalazVersion1("2.8.0")
  Console println scalazVersion2("2.8.0")
  println

  Console println scalazVersion1("2.8.0")
  Console println scalazVersion2("2.8.0")
  println

  Console println scalazVersion1("2.9.0")
  Console println scalazVersion2("2.9.0")
  println

  Console println scalazVersion1("2.9.0.1")
  Console println scalazVersion2("2.9.0.1")
  println
}
