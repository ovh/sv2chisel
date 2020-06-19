// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

lazy val ScalaTestVersion = "3.0.5"
lazy val AntlrVersion     = "4.7.1"

lazy val root = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(
    inThisBuild(
      List(
        organization := "com.ovhcloud",
        scalaVersion := "2.12.10",
        version := "0.1.0-SNAPSHOT"
      )),
    name := "sv2chisel",
    
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
      "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
      "org.antlr"     % "antlr4"     % AntlrVersion,
      "org.apache.commons" % "commons-text" % "1.7"
    ),
    antlr4GenListener in Antlr4 := false,
    antlr4GenVisitor in Antlr4 := true,
    antlr4PackageName in Antlr4 := Option("sv2chisel.antlr"),
    scalacOptions ++= Seq(
      "-deprecation"
    ),
    
    // Compile / unmanagedResources / includeFilter := "*.sv"
    includeFilter in Compile in unmanagedResources  := "*.sv"
  )
