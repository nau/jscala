import java.awt.Desktop

lazy val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
lazy val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
lazy val buildSettings = Seq(
    organization := "org.jscala",
    version := "0.6-SNAPSHOT",
    crossScalaVersions := Seq("2.11.12", "2.12.11", "2.13.2"),
    scalaVersion := "2.13.2",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    usePgpKeyHex("2B6E37353BE8BF8ED89B858DBC5373CC0297421A"),
    publishTo := { Some( if (version.value.trim endsWith "SNAPSHOT") ossSnapshots else ossStaging)},
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := (_ => false),
    pomExtra := extraPom,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
//      "-Ystatistics",
//      "-verbose",
      "-language:_"
	  ),
    Compile / scalacOptions ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 13 => "-Ymacro-annotations" :: Nil
        case _ => Nil
      }
    },
    libraryDependencies ++= {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, n)) if n >= 13 => Nil
        case _ => compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full) :: Nil
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

val tetris = taskKey[Unit]("Translates tetris Scala code to Javascript and runs the game")

lazy val root = (project in file(".")).settings(buildSettings: _*).settings(
  name := "jscala",
  crossScalaVersions := Nil
) aggregate(jscala, jscalaAnnots, examples)

lazy val jscala = (project in file("jscala")).settings(buildSettings:_*).settings(
  name := "jscala-macros",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
  libraryDependencies += "org.scala-js" % "scalajs-library_2.11" % "0.6.15",
  libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.8" % "provided"
)

lazy val jscalaAnnots = (project in file("jscala-annots")).settings(buildSettings: _*).settings(
  name := "jscala-annots",
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided"
).dependsOn(jscala)

lazy val examples: Project = (project in file("jscala-examples")).settings(buildSettings: _*).settings(
  name := "jscala-examples",
  tetris := {
    runner.value.run(
      "org.jscalaexample.Tetris",
      Attributed.data((Runtime / fullClasspath).value), Seq((baseDirectory.value / "javascript-tetris" / "tetris.js").toString),
      streams.value.log
    )
    Desktop.getDesktop.browse(baseDirectory.value / "javascript-tetris" / "index.html" toURI)
  },
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.3" % "test",
  libraryDependencies += "org.scala-js" % "scalajs-dom_sjs0.6_2.11" % "0.9.1",
  libraryDependencies += "be.doeraene" % "scalajs-jquery_sjs0.6_2.11" % "0.9.1",
  libraryDependencies += "com.typesafe.play" %% "play-json" % "2.7.4",
  libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.8"
).dependsOn(jscalaAnnots)
