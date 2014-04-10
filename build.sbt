organization := "com.kolor"

name := "reactive-docker"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full)

publishMavenStyle := true

publishTo <<= version { (v: String) =>
  val nexus = "https://oss.sonatype.org/"
  if (v.trim.endsWith("SNAPSHOT")) 
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

parallelExecution in Test := false

publishArtifact in Test := false

pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://github.com/almoehi/reactive-docker</url>
  <licenses>
    <license>
      <name>Apache 2</name>
      <url>http://www.apache.org/licenses/LICENSE-2.0.html</url>
      <distribution>repo</distribution>
    </license>
  </licenses>
  <scm>
    <url>git@github.com:almoehi/reactive-docker.git</url>
    <connection>scm:git:git@github.com:almoehi/reactive-docker.git</connection>
  </scm>
  <developers>
    <developer>
      <id>almoehi</id>
      <name>alm oehi</name>
      <url>http://github.com/almoehi/</url>
    </developer>
  </developers>)

scalacOptions ++= Seq("-deprecation","-language:_")

// scalacOptions in (Compile, console) += "-Xlog-implicits"


  javacOptions ++= Seq("-target", "1.6", "-source","1.6")

val logbackVer = "1.0.9"

libraryDependencies ++= Seq(
            //"org.scalaz.stream" %% "scalaz-stream" % "0.3.1",
            "com.netaporter" %% "scala-uri" % "0.4.1",
            "com.typesafe.play" %% "play-json" % "2.2.2",
            "com.typesafe.play" %% "play-iteratees" % "2.2.2",
            // "com.typesafe.akka" %% "akka-actor" % "2.4-SNAPSHOT",
            "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
            "org.specs2" %% "specs2" % "2.3.10" % "test",
            "ch.qos.logback" % "logback-core" % logbackVer,
            "ch.qos.logback" % "logback-classic" % logbackVer
  )

// see https://github.com/typesafehub/scalalogging/issues/23
testOptions in Test += Tests.Setup(classLoader =>
  classLoader
    .loadClass("org.slf4j.LoggerFactory")
    .getMethod("getLogger", classLoader.loadClass("java.lang.String"))
    .invoke(null, "ROOT")
)

resolvers ++= Seq(
    Resolver.mavenLocal,
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.typesafeRepo("releases"),
    Resolver.typesafeRepo("snapshots")
    //"Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases",
    //"spray repo" at "http://repo.spray.io"
    )


testOptions in Test += Tests.Argument("-oDF")
