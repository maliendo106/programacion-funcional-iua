ThisBuild / scalaVersion := "3.3.1"

lazy val tp08 = (project in file("."))
  .settings(
    name := """Práctico 8""",
    version := "3.23.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % "test",
    run / fork := true,
    Test / logBuffered := false,
    autoAPIMappings := true,
    scalacOptions ++= Seq(
      "-feature",
      "-deprecation",
      "-explain",
      "-explain-types"
    ),
    Compile / doc / scalacOptions ++= Seq(
      "-doc-root-content",
      baseDirectory.value + "/root-doc.txt"
    )
  )
