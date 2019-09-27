resolvers += ("Typesafe repo" at "http://repo.typesafe.com/typesafe/releases/").withAllowInsecureProtocol(true)

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.4")
addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat" % "1.0.9")
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.5.1")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")
addSbtPlugin("com.timushev.sbt" % "sbt-updates" % "0.4.0")
addSbtPlugin("com.softwaremill.clippy" % "plugin-sbt" % "0.6.1")
addSbtPlugin("com.etsy" % "sbt-compile-quick-plugin" % "1.4.0")
