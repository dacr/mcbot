name := "mcbot"
version := "0.0.1-SNAPSHOT"
isSnapshot := true

scalaVersion := "2.11.6"
scalacOptions ++= Seq("-unchecked", "-deprecation" , "-feature")

mainClass in assembly := Some("mcbot.MCBot")
jarName in assembly := "mcbot.jar"

libraryDependencies ++= Seq(
  "org.spacehq"       % "mcprotocollib"  % "1.8-SNAPSHOT",
  "com.github.scopt" %% "scopt"          % "3.3.0",
  "org.scalatest"    %% "scalatest"      % "2.2.+"  % "test"
  )

resolvers += Resolver.sonatypeRepo("public")
resolvers += "spacehq snapshots" at "http://repo.spacehq.org/content/repositories/snapshots"

initialCommands in console := """
   |import dummy._
   |""".stripMargin

