val JenaVersion = "3.17.0"

ThisBuild / scalaVersion     := "2.13.11"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "sparql-param-demo",
    libraryDependencies ++= Seq(
      "org.apache.jena" % "jena-core"   % JenaVersion,
      "org.apache.jena" % "jena-arq"    % JenaVersion,
      "org.scalatest"   %% "scalatest"  % "3.2.15"  % Test
    )
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
