// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

lazy val ScalaVersion       = "2.12.15"
lazy val CrossScalaVersions = Seq("2.13.7", "2.12.15")
lazy val ChiselVersion      = "3.5.0"

lazy val ScalaTestVersion   = "3.2.2"
lazy val AntlrVersion       = "4.7.1"
lazy val CirceVersion       = "0.14.1"

lazy val commonSettings = Seq (
  // identity
  versionScheme := Some("early-semver"),
  organization := "com.ovhcloud",
  organizationName := "ovhcloud",
  organizationHomepage := Some(url("https://ovhcloud.com/")),
  // versions
  scalaVersion := ScalaVersion,
  crossScalaVersions := CrossScalaVersions,
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
  Compile / doc / scalacOptions ++= Seq(
    "-feature",
    "-diagrams",
    "-diagrams-max-classes", "250",
    "-doc-version", version.value,
    "-doc-title", name.value,
    "-sourcepath", (ThisBuild / baseDirectory).value.toString,
    "-doc-source-url", {
      val branch =
        if (version.value.endsWith("-SNAPSHOT")) {
          "master"
        } else {
          s"v${version.value}"
        }
      s"https://github.com/ovh/sv2chisel/tree/$branch€{FILE_PATH_EXT}#L€{FILE_LINE}"
    }
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
    Antlr4 / antlr4GenListener := false,
    Antlr4 / antlr4GenVisitor := true,
    Antlr4 / antlr4PackageName := Option("sv2chisel.antlr")
  )
  // SBT-RELEASE SETTINGS
  .settings(
    releaseIgnoreUntrackedFiles := true,
    releaseVcsSign := true,
    releaseVcsSignOff := true,
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      releaseStepCommandAndRemaining("+clean"),
      releaseStepCommandAndRemaining("+test"),
      releaseStepCommandAndRemaining("+helpers/clean"),
      releaseStepCommandAndRemaining("+helpers/test"),
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      // For non cross-build projects, use releaseStepCommand("publishSigned")
      releaseStepCommandAndRemaining("+publishSigned"),
      releaseStepCommand("sonatypeBundleRelease"),
      releaseStepCommand("project helpers"), // need to change project to make sonatypeBundleRelease available
      releaseStepCommand("sonatypeBundleClean"), // avoid confusion with previous publishSigned files, bug?
      releaseStepCommandAndRemaining("+publishSigned"),
      releaseStepCommand("sonatypeBundleRelease"),
      releaseStepCommand("sonatypeBundleClean"),
      releaseStepCommand("project root"), // back to work
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )
  // SBT-ASSEMBLY SETTINGS 
  .settings(
    assembly / mainClass := Some("com.ovhcloud.sv2chisel.Main"),
    assembly / assemblyJarName := "sv2chisel.jar",
    assembly / test := {},
    assembly / assemblyOutputPath := file("./utils/bin/sv2chisel.jar")
  )
  
lazy val helpers = (project in file("helpers"))
  .settings(commonSettings: _*)
  .settings(
    name := "sv2chisel-helpers",
    scalacOptions += "-language:reflectiveCalls", //reflective access to structural type members (Bundle fields)
    addCompilerPlugin("edu.berkeley.cs" % "chisel3-plugin" % ChiselVersion cross CrossVersion.full), // naming
    libraryDependencies ++= Seq(
      "edu.berkeley.cs" %% "chisel3" % ChiselVersion
    )
  )


