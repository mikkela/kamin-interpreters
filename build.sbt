ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.2"

lazy val commonDependencies: Set[ModuleID] = Set(
  "org.jline" % "jline" % "3.30.5",
  "org.scalameta" %% "munit" % "1.1.1" % Test
)

lazy val root = (project in file("."))
  .settings(
    name := "kamin",
    idePackagePrefix := Some("com.mikadocs.kamin"),
    libraryDependencies ++= commonDependencies.toSeq,
    testFrameworks += new TestFramework("munit.Framework")
  )

