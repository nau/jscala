import sbt._
import sbt.Keys._

object BuildSettings {
  val ossSnapshots = "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
  val ossStaging   = "Sonatype OSS Staging" at "https://oss.sonatype.org/service/local/staging/deploy/maven2/"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.jscala",
    version := "0.2-SNAPSHOT",
    scalaVersion := "2.10.2",
    resolvers += Resolver.sonatypeRepo("snapshots"),
    credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
    publishTo <<= version((v: String) => Some( if (v.trim endsWith "SNAPSHOT") ossSnapshots else ossStaging)),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := (_ => false),
    pomExtra := extraPom,
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
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

  lazy val root: Project = Project(
    "jscala",
    file("."),
    settings = buildSettings
  ) aggregate(jscala, examples)
  


  lazy val jscala: Project = Project(
    "jscala-macros",
    file("jscala"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies += "com.yahoo.platform.yui" % "yuicompressor" % "2.4.7"
    )
  )

  lazy val examples: Project = Project(
    "jscala-examples",
    file("jscala-examples"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq("org.scalatest" %% "scalatest" % "1.9.1" % "test")
    )
  ) dependsOn(jscala)
}

