
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.0" cross CrossVersion.full)

lazy val common = project
  .in(file("."))
  .settings(
    scalaVersion := "2.13.6",
    scalacOptions := Seq(
      "-Ymacro-annotations",
      "-Wconf:any:wv",
      "-language:implicitConversions",
      "-Xlog-implicits",
//      "-Ymacro-debug-verbose",
      "-language:higherKinds",
      "-language:existentials",
      "-Ywarn-unused",
      "-Ywarn-dead-code",
      "-Xlint:-adapted-args,_",
      "-unchecked",
      "-feature",
      "-encoding",
      "utf8"
    ),
    cancelable in Global := true,
//    clippyColorsEnabled := true,
    version := "0.1.0",
//    scapegoatVersion in ThisBuild := "1.3.8",
    run / fork := true,
    name := "scala-exp",
    libraryDependencies ++= Seq(
//      "org.scala-lang"    % "scala-library-all"       % "2.13.6" pomOnly(),
      "org.typelevel"     %% "cats-effect"            % "2.5.3",
      "org.typelevel"     %% "cats-core"              % "2.6.1",
      "com.github.cb372"  %% "cats-retry-core"        % "0.3.2",
      "com.github.cb372"  %% "cats-retry-cats-effect" % "0.3.0",
      "com.chuusai"       %% "shapeless"              % "2.3.7" ,
      "io.7mind.izumi" %% "distage-testkit-scalatest" % "1.0.8" % Test

    )
  )
