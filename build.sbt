val scala3Version = "3.2.0"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "scala3-mustache",
    version      := "0.1.2.2",
    organization := "com.anglypascal",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"     % "0.7.29" % Test,
      "org.scalatest" %% "scalatest" % "3.2.12"  % Test,
      "com.rallyhealth" %% "weepickle-v1" % "1.7.2",
    )
  )

scalacOptions ++= Seq(
  "-deprecation",
  "-feature"
  /* "-explain" */
)
