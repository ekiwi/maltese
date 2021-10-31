organization := "edu.berkeley.cs"
name := "maltese"
version := "0.5-SNAPSHOT"

// scala settings
scalaVersion := "2.13.5"
crossScalaVersions := Seq("2.12.13", "2.13.5")
scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked")
// Scala 2.12 requires Java 8.
javacOptions ++= Seq("-source", "1.8", "-target", "1.8")


// JNA for SMT Solver bindings
libraryDependencies += "net.java.dev.jna" % "jna" % "5.4.0"
libraryDependencies += "net.java.dev.jna" % "jna-platform" % "5.4.0"

// BDD library
libraryDependencies += "com.github.com-github-javabdd" % "com.github.javabdd" % "1.0.1"
// test dependencies
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.6" % "test"

resolvers += Resolver.sonatypeRepo("snapshots")
libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.5-SNAPSHOT"

scalaSource in Compile := baseDirectory.value / "src"
scalaSource in Test := baseDirectory.value / "test"
resourceDirectory in Test := baseDirectory.value / "test" / "resources"
