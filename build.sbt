ThisBuild / scalaVersion := "3.4.2"
ThisBuild / organization := "dev.buoy"
ThisBuild / version      := "0.1.0-SNAPSHOT"

val zioVersion = "2.1.24"

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Wunused:all",
    "-Wvalue-discard",
    "-Wnonunit-statement",
    "-Xfatal-warnings"
  ),
  libraryDependencies ++= Seq(
    "dev.zio" %% "zio"          % zioVersion,
    "dev.zio" %% "zio-test"     % zioVersion % Test,
    "dev.zio" %% "zio-test-sbt" % zioVersion % Test
  ),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

lazy val core = project
  .in(file("modules/core"))
  .settings(commonSettings)
  .settings(name := "buoy-core")

lazy val proxy = project
  .in(file("modules/proxy"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(name := "buoy-proxy")

lazy val queue = project
  .in(file("modules/queue"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(name := "buoy-queue")

lazy val fanout = project
  .in(file("modules/fanout"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(name := "buoy-fanout")

lazy val cli = project
  .in(file("modules/cli"))
  .dependsOn(proxy, queue, fanout)
  .settings(commonSettings)
  .settings(name := "buoy-cli")

lazy val root = project
  .in(file("."))
  .aggregate(core, proxy, queue, fanout, cli)
  .settings(
    name           := "buoy",
    publish / skip := true
  )
