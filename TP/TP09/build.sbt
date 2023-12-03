ThisBuild / scalaVersion := "3.3.1"

lazy val tp09 = (project in file("."))
  .dependsOn(tp08)
  .settings(
    name := """Practico9""",
    version := "3.23.10",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.17" % "test",
    libraryDependencies += "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % "test",
    run / fork := true,
    Test / logBuffered := false,
    autoAPIMappings := true,
    scalacOptions ++= Seq(
      "-source:future",
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
lazy val tp08 = ProjectRef(file("../TP08"), "tp08")
