import sbt._
import sbt.Keys._

lazy val circe_pad = project.in(file("."))

organization := "es.weso"

name := "circe_pad"

version := "0.0.1"

scalaVersion := "2.11.8"

publishMavenStyle := true

val circeVersion = "0.5.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)


libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0-RC2" % "test",
  "org.typelevel" %% "cats" % "0.7.0",
  "es.weso" % "srdf-jena_2.11" % "0.0.8" 
  )

// to write types like Reader[String, ?]
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

// to get types like Reader[String, ?] (with more than one type parameter) correctly inferred
addCompilerPlugin("com.milessabin" % "si2712fix-plugin_2.11.8" % "1.2.0")

bintrayRepository in bintray := "weso-releases"

bintrayOrganization in bintray := Some("weso")

licenses += ("MPL-2.0", url("http://opensource.org/licenses/MPL-2.0"))

resolvers += "Bintray" at "http://dl.bintray.com/weso/weso-releases"

