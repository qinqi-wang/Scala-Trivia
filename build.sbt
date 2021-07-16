scalaVersion := "2.13.6"

scalacOptions ++= Seq("-feature", "-deprecation")

resolvers += DefaultMavenRepository

libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.1" % "test"
