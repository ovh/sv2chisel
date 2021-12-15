// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

lazy val ScalaVersion     = "2.12.10"
lazy val ChiselVersion    = "3.4.3"

lazy val ScalaTestVersion = "3.2.2"
lazy val AntlrVersion     = "4.7.1"
lazy val CirceVersion     = "0.14.1"

lazy val commonSettings = Seq (
  // identity
  organization := "com.ovhcloud",
  organizationName := "ovhcloud",
  organizationHomepage := Some(url("https://ovhcloud.com/")),
  // versions
  scalaVersion := ScalaVersion,
  version := "0.1.0-RC1",
  // dependencies
  libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % ScalaTestVersion % Test,
    "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
    // escape utils 
    "org.apache.commons" % "commons-text" % "1.7"
  ),
  // compile options
  scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked", 
    "-feature",
    "-Ywarn-unused",
    "-language:implicitConversions"
  ),
  // documentation compile options 
  scalacOptions in Compile in doc ++= Seq(
    "-feature",
    "-diagrams",
    "-diagrams-max-classes", "250",
    "-doc-version", version.value,
    "-doc-title", name.value,
    "-sourcepath", (baseDirectory in ThisBuild).value.toString
  ),
  // publication (see sonatype.sbt for further details)
  sonatypeCredentialHost := "s01.oss.sonatype.org",
  sonatypeProfileName := "com.ovhcloud",
  publishMavenStyle := true,
  licenses := List(
    "BSD-3-Clause" -> new URL("https://raw.githubusercontent.com/ovh/sv2chisel/master/LICENSE"),
    "MIT" -> new URL("https://raw.githubusercontent.com/ovh/sv2chisel/master/LICENSE.Nic30"),
    "BSD-4-Clause-UC" -> new URL("https://raw.githubusercontent.com/ovh/sv2chisel/master/LICENSE.firrtl")
  ),

  homepage := Some(url("https://github.com/ovh/sv2chisel")),
  scmInfo := Some(
    ScmInfo(
      url("https://github.com/ovh/sv2chisel"),
      "scm:git@github.com:ovh/sv2chisel.git"
    )
  ),
  developers := List(
    Developer(
      id    = "johnsbrew",
      name  = "Jean Bruant",
      email = "jean.bruant@ovhcloud.com",
      url   = url("https://github.com/johnsbrew")
    )
  ),
  usePgpKeyHex("D135C408D8400669B906F92F358CDB999C674A51"),
  publishTo := sonatypePublishToBundle.value
)

import ReleaseTransformations._

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
  .settings(
    releaseIgnoreUntrackedFiles := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runClean,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      // For cross-build projects, use releaseStepCommandAndRemaining("+publishSigned")
      releaseStepCommand("publishSigned"),
      releaseStepCommand("sonatypeBundleRelease"),
      releaseStepCommand("project helpers"), // need to change project to make sonatypeBundleRelease to work
      releaseStepCommand("publishSigned"),
      releaseStepCommand("sonatypeBundleRelease"),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )
  
lazy val helpers = (project in file("helpers"))
  .settings(commonSettings: _*)
  .settings(
    name := "sv2chisel-helpers",
    
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % ChiselVersion
    )
  )


