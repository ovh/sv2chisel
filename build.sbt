// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
// Copyright 2020 The sv2chisel Authors. All rights reserved.

lazy val ScalaVersion       = "2.12.15"
lazy val CrossScalaVersions = Seq("2.13.7", "2.12.15")
lazy val ScalaNativeVersion = "2.13.7"
// no need to follow latest patch as binary compatibility between patches is guaranteed by Chisel maintainers
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

lazy val sv2chiselSettings = commonSettings ++ Seq(
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
lazy val releaseSettings = Seq(
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
    releaseStepCommand("sonatypeBundleClean"), // avoid confusion with previous publishSigned files, bug?
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
lazy val asmSettings = Seq(
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

lazy val root = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(sv2chiselSettings: _*)
  .settings(releaseSettings: _*)
  .settings(asmSettings: _*)


///////// Native-image local setup notes ///////
// MacOs: xcode-select --install ; then out-of-the-box
// Linux: a bunch of basic native packages, see https://www.graalvm.org/22.0/reference-manual/native-image/ ; then out-of-the-box
// Windows: 
//   - VS Code setup with Win 10 SDK ; MSVC vXXX x64/x86 C++, see https://medium.com/graalvm/using-graalvm-and-native-image-on-windows-10-9954dc071311
//   - for nativeImage command to succeed, sbt must be launched from "x64 Native Tools Command Prompt for VS 20XX"
//   - nativeImageRun fails because nativeImage generates the executable with '.exe' file extension whereas nativeImageRun looks for the executable without extension 

// IMPORTANT: root project must be commented out to make natimg project visible ... could not find a nicer way ...
// Native image is not that easy to get right
// sbt native image plugin fixes a bunch of issues however is does not work properly for scala 2.12
// specifically the underlying lib providing the fix is not fetched properly (not with _2.12)
// library with the fix: svm-subs: https://github.com/scalameta/svm-subs
// NB: It happens to work with manual fix as in: https://github.com/plokhotnyuk/jsoniter-scala/commit/e089f06c2d8b4bdb87a6874e17bf716e8608b117
// see discussion: https://github.com/scala/bug/issues/11634
// with the addition of --initialize-at-build-time=scala.collection.immutable.VM
// CONCLUSION: we build the image based on the java generated by scala 2.13, as scala 2.12 will be deprecated at some point 
lazy val natimg = (project in file("."))
  .enablePlugins(Antlr4Plugin)
  .settings(sv2chiselSettings: _*)

  .settings(
    scalaVersion := ScalaNativeVersion,
    crossScalaVersions := Seq(ScalaNativeVersion),
    Compile / mainClass := Some("sv2chisel.Main"),
    // Reflection usage in sv2chisel is only due to circe library (on some parts we do not actually use)
    // However, let's generate the config properly not to be forced to use the dirty option --report-unsupported-elements-at-runtime
    nativeImageOptions += s"-H:ReflectionConfigurationFiles=${target.value / "native-image-configs" / "reflect-config.json"}",
    nativeImageOptions += s"-H:ConfigurationFileDirectories=${target.value / "native-image-configs" }",
    nativeImageOptions +="-H:+JNI",
    nativeImageOptions +="--no-fallback",
    
    // due to some sbt magic, the expected dependency on nativeImageCommand is not necessarily fullfilled 
    // this should be fixed in sbt-native-image plugin version 0.3.2
    addCommandAlias("imgBuild", "; nativeImageCommand ;nativeImageRunAgent \" -c src/main/resources/project/config.yml -l warn\" ;nativeImage"),
    addCommandAlias("imgTest", ";nativeImageRun -c src/main/resources/project/config.yml -l info")
  )
  .enablePlugins(NativeImagePlugin)


