lazy val root = (project in file(".")).settings(
  name := "number-aware-comparison",
  scalaVersion := "2.12.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)