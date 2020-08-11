name := "maltese"
version := "0.1"
scalaVersion := "2.13.2"

// utest
libraryDependencies += "com.lihaoyi" %% "utest" % "0.7.2" % "test"
testFrameworks += new TestFramework("utest.runner.Framework")

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
