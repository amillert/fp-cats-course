Global / onChangedBuildSource := ReloadOnSourceChanges

scalaVersion := "2.13.1"
ThisBuild / organization := "pl.amillert"

lazy val root = (project in file("."))
  .settings(name := "root")

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-core"            % "2.1.1"
libraryDependencies += "org.typelevel" %% "cats-laws"            % "2.1.1"
libraryDependencies += "org.typelevel" %% "discipline-core"      % "1.0.0"
libraryDependencies += "org.typelevel" %% "discipline-scalatest" % "2.1.1"
libraryDependencies += "org.scalatest" %% "scalatest"            % "3.2.2"
