import com.typesafe.sbt.SbtStartScript

seq(SbtStartScript.startScriptForClassesSettings: _*)

ScoverageSbtPlugin.instrumentSettings

CoverallsPlugin.coverallsSettings

organization := "uk.co.mattthomson.coursera.ggp"

name := "gresley"

version := "0.0.1"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.2.2",
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.14.v20131031",
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" artifacts (Artifact("javax.servlet", "jar", "jar")),
  "com.github.nscala-time" %% "nscala-time" % "1.0.0",
  "com.twitter" %% "util-core" % "6.16.0"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "1.9.1" % "test",
  "org.scalatra" %% "scalatra-scalatest" % "2.2.2" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.1.2" % "test"
)
