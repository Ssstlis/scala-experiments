import com.sksamuel.scapegoat.sbt.ScapegoatSbtPlugin.autoImport._
import com.softwaremill.clippy.ClippySbtPlugin.autoImport.clippyColorsEnabled


addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

lazy val common = project
  .in(file("."))
  .settings(
    scalaVersion := "2.12.10",
    scalacOptions := Seq(
      "-language:implicitConversions",
        "-Xlog-implicits",
      "-language:higherKinds",
      "-language:existentials",
      "-Ywarn-unused",
      "-Ypartial-unification",
      "-Ywarn-dead-code",
      "-Xlint:-adapted-args,_",
      "-deprecation",
      "-unchecked",
      "-feature",
      "-encoding",
      "utf8"
    ),
    cancelable in Global := true,
    clippyColorsEnabled := true,
    version := "0.1.0",
    scapegoatVersion in ThisBuild := "1.3.8",
    fork in run := true,
    sources in(Compile, doc) := Seq.empty,
    publishArtifact in(Compile, packageDoc) := false,
    name := "scala-exp",
    libraryDependencies ++= Seq(
      "org.scala-lang"    % "scala-library-all"       % "2.12.10" pomOnly(),
      "org.typelevel"     %% "cats-effect"            % "2.0.0",
      "org.typelevel"     %% "cats-core"              % "2.0.0",
      "com.github.cb372"  %% "cats-retry-core"        % "0.3.0",
      "com.github.cb372"  %% "cats-retry-cats-effect" % "0.3.0",
      "com.chuusai"       %% "shapeless"              % "2.3.3" ,

    )
  )
