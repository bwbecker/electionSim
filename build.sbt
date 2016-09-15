name := "election_sim2"

version := "1.0"

scalaVersion := "2.11.8"

lazy val root = (project in file(".")).enablePlugins(SbtWeb)

includeFilter in (Assets, LessKeys.less) := "main.less"

val uwResolver = "CS-OAT@cs.uwaterloo.ca" at "https://cs.uwaterloo.ca/~cs-oat/maven/"

val enumeratumVersion = "1.4.9"

resolvers += Resolver.sonatypeRepo("releases")

resolvers += uwResolver


libraryDependencies ++= Seq(
  "com.lihaoyi" %% "utest" % "0.4.3" % "test",
  "com.lihaoyi" %% "scalatags" % "0.5.5",
  "com.lihaoyi" %% "upickle" % "0.4.1",
  "ca.bwbecker" %% "bwblib" % "1.2",
  "com.beachape" %% "enumeratum" % enumeratumVersion,
  "com.beachape" %% "enumeratum-upickle" % enumeratumVersion

)


testFrameworks += new TestFramework("utest.runner.Framework")

scalacOptions ++= Seq("-feature", "-deprecation", "-unchecked", "-language:reflectiveCalls")
