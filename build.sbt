// See README.md for license details.

ThisBuild / scalaVersion     := "2.13.15"
ThisBuild / version          := "0.1.0"
ThisBuild / organization     := "tech.rocksavage"
ThisBuild / organizationName := "Rocksavage Technology"

Test / parallelExecution := false

lazy val synth = RootProject(
  uri("https://github.com/The-Chiselers/synth.git#main")
)
lazy val addrdecode = RootProject(
  uri("https://github.com/The-Chiselers/addrdecode.git#main")
)
lazy val apb = RootProject(uri("https://github.com/The-Chiselers/apb.git#main"))
lazy val registermap = RootProject(
  uri("https://github.com/The-Chiselers/registermap.git#main")
)
lazy val test_utils = RootProject(
  uri("https://github.com/The-Chiselers/test_utils.git#main")
)

lazy val root = (project in file("."))
    .settings(
      name                   := "i2c",
      Test / publishArtifact := true,
      libraryDependencies ++= Seq(
        "org.chipsalliance" %% "chisel"     % chiselVersion,
        "edu.berkeley.cs"   %% "chiseltest" % "6.0.0",
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
      ),
      coverageExcludedFiles :=  ".*GenVerilog;.*BaseParams;.*Interfaces;.*I2CFVHarness"
    )
    .dependsOn(
      synth,
      addrdecode,
      apb,
      registermap,
      test_utils
    )
val chiselVersion   = "6.6.0"
val scalafmtVersion = "2.5.0"

// Scala coverage settings
coverageDataDir            := target.value 
coverageFailOnMinimum      := false
coverageMinimumStmtTotal   := 0
coverageMinimumBranchTotal := 0
