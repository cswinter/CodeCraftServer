lazy val sharedDependencies =
  libraryDependencies ++= Seq(
    "org.codecraftgame" %%% "codecraft" % "0.6.1" changing (),
    "com.lihaoyi" %%% "scalatags" % "0.6.7",
    "com.lihaoyi" %% "upickle" % "0.4.1"
  )

lazy val server = (project in file("server"))
  .settings(commonSettings)
  .settings(
    scalaJSProjects := Seq(client),
    pipelineStages in Assets := Seq(scalaJSPipeline),
    pipelineStages := Seq(digest, gzip),
    // triggers scalaJSPipeline when using compile or continuous compilation
    compile in Compile := ((compile in Compile) dependsOn scalaJSPipeline).value,
    libraryDependencies ++= Seq(
      "com.vmunier" %% "scalajs-scripts" % "1.1.2",
      "javax.xml.bind" % "jaxb-api" % "2.3.0",
      guice,
      specs2 % Test
    ),
    sharedDependencies
  )
  .enablePlugins(PlayScala)
  .dependsOn(sharedJvm)

lazy val client = (project in file("client"))
  .settings(commonSettings)
  .settings(
    scalaJSUseMainModuleInitializer := true,
    libraryDependencies ++= Seq(
      "org.scala-js" %%% "scalajs-dom" % "0.9.5"
    ),
    sharedDependencies
  )
  .enablePlugins(ScalaJSPlugin, ScalaJSWeb)
  .dependsOn(sharedJs)

lazy val shared = crossProject
  .crossType(CrossType.Pure)
  .in(file("shared"))
  .settings(commonSettings)
lazy val sharedJvm = shared.jvm
lazy val sharedJs = shared.js

lazy val commonSettings = Seq(
  scalaVersion := "2.11.8",
  organization := "com.clemenswinter"
)

// loads the server project at sbt startup
onLoad in Global := (onLoad in Global).value andThen { s: State =>
  "project server" :: s
}
