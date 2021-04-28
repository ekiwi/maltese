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

// BDD library
libraryDependencies += "com.github.com-github-javabdd" % "com.github.javabdd" % "1.0.1"

// scalatest has the best IntelliJ support
libraryDependencies += "org.scalatest" %% "scalatest-flatspec" % "3.2.1" % "test"

// fork tests as native libraries aren't necessarily thread safe
Test / fork := true

// antlr for youverify parsing
antlr4GenVisitor in Antlr4 := true
antlr4GenListener in Antlr4 := false
antlr4PackageName in Antlr4 := Option("firrtl.antlr")
antlr4Version in Antlr4 := "4.8"
javaSource in Antlr4 := baseDirectory.value / "antlr4"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
