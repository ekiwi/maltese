name := "maltese"
version := "0.1"
scalaVersion := "2.13.3"

// turn on some warnings
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-feature",
)

// JNA for SMT Solver bindings
libraryDependencies += "net.java.dev.jna" % "jna" % "5.4.0"
libraryDependencies += "net.java.dev.jna" % "jna-platform" % "5.4.0"

// scalatest has the best IntelliJ support
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.1" % "test"

// fork tests as native libraries aren't necessarily thread safe
Test / fork := true

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
