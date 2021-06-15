def crossPlugin(x: sbt.librarymanagement.ModuleID) = compilerPlugin(x cross CrossVersion.full)

val compilerPlugins = List(
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1"),
  crossPlugin("org.typelevel" % "kind-projector" % "0.13.0"),
  crossPlugin("com.github.cb372" % "scala-typed-holes" % "0.1.9")
)

val commonSettings = Seq(
  scalaVersion := "2.13.6",
  scalacOptions ~= (_.filterNot(_ == "-Xfatal-warnings") ++ Seq(
    "-Ymacro-annotations",
    "-Yimports:" ++ List(
      "scala",
      "scala.Predef",
      // "cats",
      "cats.implicits",
      // "cats.effect",
      // "cats.effect.implicits",
      // "cats.effect.concurrent"
    ).mkString(",")
  )),
  // Test / fork := true,
  // updateOptions := updateOptions.value.withGigahorse(false),
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.6.1",
    "org.typelevel" %% "cats-laws" % "2.6.1",
    "org.typelevel" %% "cats-effect" % "3.1.1",
    "org.typelevel" %% "cats-effect-laws" % "3.1.1" % Test,
    "org.typelevel" %% "cats-mtl" % "1.2.0",
    "org.typelevel" %% "cats-tagless-macros" % "0.14.0",
    "org.typelevel" %% "cats-testkit-scalatest" % "2.1.5" % Test,
    // "dev.profunktor" %% "console4cats" % "0.8.1", // "0.9.0",
    "dev.profunktor" %% "redis4cats-effects" %  "1.0.0RC1",
    "dev.profunktor" %% "redis4cats-log4cats" % "1.0.0RC1",
    "io.chrisdavenport" %% "log4cats-noop" % "1.1.1"
  ) ++ compilerPlugins
)

val code =
  project.in(file("."))
    .settings(commonSettings)
    .settings(publish / skip := true)
