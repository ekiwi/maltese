name := "maltese"
version := "0.1"
scalaVersion := "2.12.10"

// required for uclid files
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.1" withSources()

// utest
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
