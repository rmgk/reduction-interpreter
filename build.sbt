scalaVersion := "2.12.6"
organization := "de.rmgk"


enablePlugins(JavaAppPackaging)

name := "tfl"

val monocleVersion = "1.5.0-cats" // 1.5.0-cats based on cats 1.0.x

libraryDependencies ++= Seq(
  "com.monovore" %% "decline" % "0.5.0",
  "com.github.pathikrit" %% "better-files" % "3.6.0",
  "com.lihaoyi" %% "fastparse" % "1.0.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test",
  "com.lihaoyi" %% "pprint" % "0.5.3",
  "com.github.julien-truffaut" %% "monocle-core" % monocleVersion,
  "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion,
  )



Compile / compile / scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-Ypatmat-exhaust-depth", "80",
  "-unchecked",
  "-feature",
  "-Xlint",
  "-Xfuture",
  //"-Xlog-implicits" ,
  "-Xfatal-warnings",
  //"-Yinline-warnings" ,
  "-Yno-adapted-args",
  //"-Ywarn-dead-code" ,
  "-Ywarn-nullary-override",
  "-Ywarn-nullary-unit",
  "-Ywarn-numeric-widen",
  //"-Ywarn-value-discard" ,
  )
