import sbt._
import Keys._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.jscala",
    version := "0.1",
    scalaVersion := "2.10.2",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-language:_"
	)
  )
}

object MyBuild extends Build {
  import BuildSettings._

  lazy val root: Project = Project(
    "jscala",
    file("."),
    settings = buildSettings
  ) aggregate(jscala, examples)
  


  lazy val jscala: Project = Project(
    "jscala-macros",
    file("jscala"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _))
  )

  lazy val examples: Project = Project(
    "jscala-examples",
    file("jscala-examples"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.9.1" % "test")
    )
  ) dependsOn(jscala)
}
