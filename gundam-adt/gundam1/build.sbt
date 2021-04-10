scalacOptions ++= Seq(
  "-Xfatal-warnings",
  "-deprecation"
)

scalaVersion := "2.13.5"

wartremoverErrors ++= Warts.unsafe
wartremoverWarnings ++= Warts.all
