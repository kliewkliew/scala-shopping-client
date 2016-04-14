name := "deputy-service-sniper"

version := "1.0"

scalaVersion := "2.11.7"

resolvers ++= Seq(
  "Spray Repository" at "http://repo.spray.io/",
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

// For Akka 2.4.x and Scala 2.11.x
libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.1",
  "io.spray" %  "spray-client" % "1.2.3",
  "org.scala-lang.modules" %% "scala-xml" % "1.0.3"
)
