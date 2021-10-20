// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

lazy val ScalaTestVersion = "3.2.2"
lazy val AntlrVersion     = "4.7.1"
lazy val CirceVersion     = "0.14.1"

lazy val commonSettings = Seq (
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases")
  ),
  organization := "com.ovhcloud",
  scalaVersion := "2.12.10",
  version := "0.1.0-SNAPSHOT",
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
    // escape utils 
    "org.apache.commons" % "commons-text" % "1.7"
  ),
  scalacOptions ++= Seq(
    "-deprecation",
    "-language:implicitConversions"
  ),
  scalacOptions in Compile in doc ++= Seq(
    "-feature",
    "-diagrams",
    "-diagrams-max-classes", "250",
    "-doc-version", version.value,
    "-doc-title", name.value,
    "-sourcepath", (baseDirectory in ThisBuild).value.toString
  )
)

lazy val root = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(commonSettings: _*)
  .settings(
    name := "sv2chisel",
    libraryDependencies ++= Seq(
      "org.antlr"     % "antlr4"     % AntlrVersion,
      // JSON/YAML parsing
      "io.circe" %% "circe-yaml" % CirceVersion,
      "io.circe" %% "circe-core" % CirceVersion,
      "io.circe" %% "circe-parser" % CirceVersion,
      // Arg parsing
      "org.sellmerfud" %% "optparse" % "2.2"
    ),
    antlr4GenListener in Antlr4 := false,
    antlr4GenVisitor in Antlr4 := true,
    antlr4PackageName in Antlr4 := Option("sv2chisel.antlr")
  )
  // .aggregate(helpers)
  
lazy val helpers = (project in file("helpers"))
  .settings(commonSettings: _*)
  .settings(
    name := "sv2chisel-helpers",
    
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % "3.3.2"
    )
  )


