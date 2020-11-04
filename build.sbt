name := "ScalaWithCats"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "2.1.0",
)

scalacOptions ++= Seq(
  "-Xfatal-warnings",
//  "-Ypartial-unification",
)

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full)
