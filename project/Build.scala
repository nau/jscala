import java.awt.Desktop
import sbt._
import sbt.Keys._

object BuildSettings {
  val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.jscala",
    version := "0.3-SNAPSHOT",
    scalaVersion := "2.10.2",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishTo <<= version((v: String) => Some( if (v.trim endsWith "SNAPSHOT") ossSnapshots else ossStaging)),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := (_ => false),
    pomExtra := extraPom,
//    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise_2.10.3-RC1" % "2.0.0-SNAPSHOT"),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
//      "-Ystatistics",
//      "-verbose",
      "-language:_"
	  )
  )

  def extraPom =
    <url>http://jscala.org</url>
      <licenses>
        <license>
          <name>MIT</name>
          <url>http://opensource.org/licenses/MIT</url>
          <distribution>repo</distribution>
        </license>
      </licenses>
      <scm>
        <url>git@github.com:nau/jscala.git</url>
        <connection>scm:git:git@github.com:nau/jscala.git</connection>
      </scm>
      <developers>
        <developer>
          <id>nau</id>
          <name>Alexander Nemish</name>
          <url>http://github.com/nau</url>
        </developer>
      </developers>
}

object JScalaBuild extends Build {
  import BuildSettings._

  val tetris = TaskKey[Unit]("tetris", "Translates tetris Scala code to Javascript and runs the game")

  val tetrisTask = tetris <<= (baseDirectory in examples, fullClasspath in Runtime, runner in run, streams) map { (bd, cp, r, s) =>
    r.run("org.jscalaexample.Tetris", Build.data(cp), Seq((bd / "javascript-tetris" / "tetris.js").toString), s.log)
    Desktop.getDesktop.browse(bd / "javascript-tetris" / "index.html" toURI)
  }

  lazy val root: Project = Project(
    "jscala",
    file("."),
    settings = buildSettings
  ) aggregate(jscala, examples)
  


  lazy val jscala: Project = Project(
    "jscala-macros",
    file("jscala"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _ % "provided"),
      libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.7"
    )
  )

  lazy val examples: Project = Project(
    "jscala-examples",
    file("jscala-examples"),
    settings = buildSettings ++ Seq(
      tetrisTask,
      libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.9.1" % "test")
    )
  ) dependsOn(jscala)
}

