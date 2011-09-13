// set the name of the project
name := "Virginia"

version := "1.0"

organization := "com.paulsnively"

// set the Scala version used for the project
scalaVersion := "2.9.1"

// add a dependency on Databinder Dispatch and on scala-time
libraryDependencies ++= Seq(
      "net.databinder" %% "dispatch-http" % "0.8.5",
        "org.scala-tools.time" %% "time" % "0.5"
    )
