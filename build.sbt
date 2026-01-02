val ScalaVersion = "3.3.4"
val ZioVersion = "2.1.14"

lazy val root = project
  .in(file("."))
  .settings(
    name := "zio-playground",
    version := "0.1.0",
    scalaVersion := ScalaVersion,
    libraryDependencies ++= Seq(
        "dev.zio" %% "zio"          % ZioVersion,
        "dev.zio" %% "zio-streams"  % ZioVersion,
        "dev.zio" %% "zio-test"     % ZioVersion % Test,
        "dev.zio" %% "zio-test-sbt" % ZioVersion % Test
      ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework"),
    scalacOptions ++= Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-Xfatal-warnings",
        "-Wunused:imports"
      )
    )

