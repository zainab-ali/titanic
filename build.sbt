import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.zainab-ali",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT",
      scalaOrganization := "org.typelevel",
      scalaVersion := "2.12.4-bin-typelevel-4"
    )),
    name := "titanic",
    libraryDependencies ++= Seq(
      purecsv,
      matryoshka,
      scalaTest % Test
    ),
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3"),
  )
