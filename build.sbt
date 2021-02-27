ThisBuild / scalaVersion := "2.13.4"
ThisBuild / organization := "com.example"

lazy val hello = (project in file(".")).settings(
    name := "Hello",
   libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.5" % "test"
  )
