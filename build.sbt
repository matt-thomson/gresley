enablePlugins(JavaAppPackaging)

coverageEnabled := true

organization := "uk.co.mattthomson.coursera.ggp"

name := "gresley"

version := "0.0.2"

scalaVersion := "2.13.0"

val AkkaVersion = "2.6.17"

libraryDependencies ++= Seq(
  "org.scalatra" %% "scalatra" % "2.7.0", // latest version uses scala-parser-combinators 2.0 incomp with util-core
  "ch.qos.logback" % "logback-classic" % "1.0.13",
  "org.eclipse.jetty" % "jetty-webapp" % "9.4.44.v20210927",
  //"org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" artifacts (Artifact("javax.servlet", "jar", "jar")),
  "com.github.nscala-time" %% "nscala-time" % "2.30.0",
  "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
  "com.twitter" %% "util-core" % "21.12.0"
)


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.10" % "test",
  "org.scalatestplus" %% "mockito-3-12" % "3.2.10.0" % "test",
  "org.scalatra" %% "scalatra-scalatest" % "2.8.1" % "test",
  "com.typesafe.akka" %% "akka-testkit" % AkkaVersion % "test"
)
