import java.awt.Desktop
import sbt._
import sbt.Keys._

object BuildSettings {
  val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
  val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "org.jscala",
    version := "0.4",
    scalaVersion := "2.11.8",
    crossScalaVersions := Seq("2.10.6", "2.11.8"),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishTo <<= version((v: String) => Some( if (v.trim endsWith "SNAPSHOT") ossSnapshots else ossStaging)),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := (_ => false),
    pomExtra := extraPom,
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
//      "-Ystatistics",
//      "-verbose",
      "-language:_"
	  ),
    initialize ~= { _ => sys.props("scalac.patmat.analysisBudget") = "10" },
    libraryDependencies := {
      CrossVersion.partialVersion(scalaVersion.value) match {
        // if scala 2.11+ is used, quasiquotes are merged into scala-reflect
        case Some((2, scalaMajor)) if scalaMajor >= 11 => libraryDependencies.value
        // in Scala 2.10, quasiquotes are provided by macro paradise
        case Some((2, 10)) =>
          libraryDependencies.value ++ Seq(
            "org.scalamacros" %% "quasiquotes" % "2.0.0" cross CrossVersion.binary)
      }
    }
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
    r.run("org.jscalaexample.Tetris", Attributed.data(cp), Seq((bd / "javascript-tetris" / "tetris.js").toString), s.log)
    Desktop.getDesktop.browse(bd / "javascript-tetris" / "index.html" toURI)
  }

  lazy val root: Project = Project(
    "jscala",
    file("."),
    settings = buildSettings
  ) aggregate(jscala, jscalaAnnots, examples)
  


  lazy val jscala: Project = Project(
    "jscala-macros",
    file("jscala"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ % "provided"),
      libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.8" % "provided"
    )
  )

  lazy val jscalaAnnots: Project = Project(
    "jscala-annots",
    file("jscala-annots"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ % "provided"),

      libraryDependencies <++= scalaVersion { sv =>
        if (sv.startsWith("2.11")) Seq("fr.apyx" %% "ts2scala-macros" % "0.3.0" exclude("org.scala-lang", "scala-reflect"))
        else Seq()
      },
      sources in Compile <<= (sources in Compile, scalaVersion, baseDirectory) map { (ss, sv, bd) =>
        if (sv.startsWith("2.11")) ss
        else ss.filter { (f: java.io.File) =>
          val path = (bd / "src/main/scala/org/jscala/typescript").getCanonicalPath
          !f.getCanonicalPath.startsWith(path)
        }
      }
    )
  ) dependsOn(jscala)

  lazy val examples: Project = Project(
    "jscala-examples",
    file("jscala-examples"),
    settings = buildSettings ++ Seq(
      tetrisTask,
      libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test",
      libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.8",
      sources in Test <<= (sources in Test, scalaVersion) map { (ss, sv) =>
        if (sv.startsWith("2.11")) ss
        else ss.filter { (f: java.io.File) => !f.getCanonicalPath.endsWith("TypescriptedTest.scala") }
      }
    )
  ) dependsOn(jscalaAnnots)
}

