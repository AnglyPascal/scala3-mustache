val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "scala3-mustache",
    version      := "0.1.1",
    organization := "com.anglypascal",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"     % "0.7.29" % Test,
      "org.scalatest" %% "scalatest" % "3.2.12"  % Test
    )
  )

scalacOptions ++= Seq(
  "-deprecation"
  /* "-explain" */
)
