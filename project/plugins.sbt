// addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "1.0.0")
// addSbtPlugin("com.geirsson"   % "sbt-scalafmt"           % "1.5.1")
// addSbtPlugin("org.scoverage"  % "sbt-scoverage"          % "1.5.1")

// maven central publication
addSbtPlugin("com.github.sbt" % "sbt-release"      % "1.1.0")
addSbtPlugin("org.xerial.sbt" % "sbt-sonatype"     % "3.9.10")
addSbtPlugin("com.github.sbt" % "sbt-pgp"          % "2.1.2")

// self-contained jar executable publication
addSbtPlugin("com.eed3si9n"   % "sbt-assembly"     % "1.1.0")
addSbtPlugin("org.scalameta"  % "sbt-native-image" % "0.3.1")
