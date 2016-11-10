scalaVersion := "2.11.8"

scalacOptions ++= Seq(
  "-encoding", "UTF-8", // 2 args
  "-feature",
  "-language:existentials",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-value-discard"
)

libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core"               % "0.3.0",
  "org.tpolecat" %% "doobie-contrib-postgresql" % "0.3.0",
  "org.tpolecat" %% "doobie-tsql-core"          % "0.1-SNAPSHOT",
  "org.tpolecat" %% "doobie-tsql-postgres"      % "0.1-SNAPSHOT",
  "org.tpolecat" %% "atto-core"                 % "0.5.1",
  "org.tpolecat" %% "atto-compat-scalaz72"      % "0.5.1"
)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.7.1")

tutSettings
tutSourceDirectory := baseDirectory.value / "tut"
tutTargetDirectory := baseDirectory.value / "tut-out"

initialCommands := """
  |import scalaz._, Scalaz._
  |import doobie.imports._
  |import doobie.contrib.postgresql.pgtypes._
  |import cofree._
  """.stripMargin.trim
