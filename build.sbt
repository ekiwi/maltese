name := "maltese"
version := "0.1"
scalaVersion := "2.13.3"

// turn on some warnings
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
)

// scalatest has the best IntelliJ support
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.1" % "test"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
