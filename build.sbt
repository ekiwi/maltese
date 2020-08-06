name := "maltese"
version := "0.1"
scalaVersion := "2.13.2"

// required for uclid files
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2"
libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2" withSources()

// utest
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
