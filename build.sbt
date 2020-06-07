name := "KnoRBA"

scalaVersion in ThisBuild := "2.13.2"

version in ThisBuild := "0.1"
resolvers in ThisBuild += Resolver.mavenLocal
resolvers in ThisBuild += Resolver.mavenCentral

lazy val scalaLib = project in file("scala-lib")

lazy val languageLib = (project in file("language-lib"))
  .dependsOn(scalaLib)

lazy val knoisCompiler = (project in file("knois-compiler"))
  .dependsOn(languageLib)
