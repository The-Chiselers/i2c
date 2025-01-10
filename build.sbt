// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.15"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "tech.rocksavage"
ThisBuild / organizationName := "Rocksavage Technology"

Test / parallelExecution := false

val chiselVersion   = "6.6.0"
val scalafmtVersion = "2.5.0"

lazy val synth = RootProject(uri("https://github.com/The-Chiselers/synth.git#main"))
lazy val addrdecode = RootProject(uri("https://github.com/The-Chiselers/addrdecode.git#main"))
lazy val apb = RootProject(uri("https://github.com/The-Chiselers/apb.git#main"))
lazy val registermap = RootProject(uri("https://github.com/The-Chiselers/registermap.git#main"))
lazy val root = (project in file("."))
  .settings(
    name                   := "i2c",
    Test / publishArtifact := true,
    libraryDependencies ++= Seq(
      "org.chipsalliance" %% "chisel"     % chiselVersion,
      "edu.berkeley.cs"   %% "chiseltest" % "5.0.0" % "test",
      "org.rogach"        %% "scallop"    % "5.2.0"
    ),
    scalacOptions ++= Seq(
      "-language:reflectiveCalls",
      "-deprecation",
      "-feature",
      "-Xcheckinit",
      "-Ymacro-annotations"
    ),
    addCompilerPlugin(
      "org.chipsalliance" % "chisel-plugin" % chiselVersion cross CrossVersion.full
    )
  )
  .dependsOn(registermap)
  .dependsOn(synth)
  .dependsOn(addrdecode)
  .dependsOn(apb)

// Scala coverage settings
coverageDataDir            := target.value / "../generated/scalaCoverage"
coverageFailOnMinimum      := true
coverageMinimumStmtTotal   := 90
coverageMinimumBranchTotal := 95
