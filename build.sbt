import java.awt.Desktop
import sbt._
import sbt.Keys._

lazy val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
lazy val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
lazy val buildSettings = Defaults.coreDefaultSettings ++ Seq(
    organization := "org.jscala",
    version := "0.5-SNAPSHOT",
    scalaVersion := "2.11.8",
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

val tetris = TaskKey[Unit]("tetris", "Translates tetris Scala code to Javascript and runs the game")

val tetrisTask = tetris <<= (baseDirectory, fullClasspath in Runtime, runner in run, streams) map { (bd, cp, r, s) =>
  r.run("org.jscalaexample.Tetris", Attributed.data(cp), Seq((bd / "javascript-tetris" / "tetris.js").toString), s.log)
  Desktop.getDesktop.browse(bd / "javascript-tetris" / "index.html" toURI)
}

lazy val root: Project = (project in file(".")).settings(buildSettings: _*).settings(
  name := "jscala"
) aggregate(jscala, jscalaAnnots, examples)
  
lazy val jscala = (project in file("jscala")).settings(buildSettings:_*).settings(
  name := "jscala-macros",
  libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-reflect" % _ % "provided"),
  libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.8" % "provided"
)

lazy val jscalaAnnots = (project in file("jscala-annots")).settings(buildSettings: _*).settings(
  name := "jscala-annots",
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
).dependsOn(jscala)

lazy val examples: Project = (project in file("jscala-examples")).settings(buildSettings: _*).settings(
  name := "jscala-examples",
  tetrisTask,
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test",
  libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.8",
  sources in Test <<= (sources in Test, scalaVersion) map { (ss, sv) =>
    if (sv.startsWith("2.11")) ss
    else ss.filter { (f: java.io.File) => !f.getCanonicalPath.endsWith("TypescriptedTest.scala") }
  }
).dependsOn(jscalaAnnots)
