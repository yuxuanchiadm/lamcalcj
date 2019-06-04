import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

ThisBuild / organization      := "org.lamcalcj"
ThisBuild / version           := "1.4.0"
ThisBuild / scalaVersion      := "2.12.8"
ThisBuild / organizationName  := "LamcalcJ"
ThisBuild / startYear         := Option(2019)
ThisBuild / developers        += Developer("yuxuanchiadm", "Yu Xuanchi", "yuxuanchiadm@126.com", url("https://github.com/yuxuanchiadm"))
ThisBuild / publishMavenStyle := true
ThisBuild / publishTo         := Option(MavenRepository("lamcalcj-maven", "https://api.bintray.com/maven/yuxuanchiadm/lamcalcj-maven/lamcalcj/;publish=1"))
ThisBuild / credentials       += Credentials(Path.userHome / ".sbt" / ".credentials")

name             := "lamcalcj-parent"
autoScalaLibrary := false
artifacts        := Seq()

lazy val ast = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("lamcalcj-ast"))
  .settings(
    name := "lamcalcj-ast",

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.7" % Test
  )
lazy val astJS = ast.js
lazy val astJVM = ast.jvm

lazy val utils = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("lamcalcj-utils"))
  .dependsOn(ast)
  .settings(
    name := "lamcalcj-utils",

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.7" % Test
  )
lazy val utilsJS = utils.js
lazy val utilsJVM = utils.jvm

lazy val parser = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("lamcalcj-parser"))
  .dependsOn(utils)
  .settings(
    name := "lamcalcj-parser",

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.7" % Test
  )
lazy val parserJS = parser.js
lazy val parserJVM = parser.jvm

lazy val compiler = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("lamcalcj-compiler"))
  .dependsOn(ast)
  .dependsOn(parser)
  .settings(
    name := "lamcalcj-compiler",

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.7" % Test
  )
lazy val compilerJS = compiler.js
lazy val compilerJVM = compiler.jvm

lazy val pretty = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("lamcalcj-pretty-print"))
  .dependsOn(ast)
  .dependsOn(utils)
  .dependsOn(compiler % Test)
  .settings(
    name := "lamcalcj-pretty-print",

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.7" % Test
  )
lazy val prettyJS = pretty.js
lazy val prettyJVM = pretty.jvm

lazy val reducer = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .in(file("lamcalcj-reducer"))
  .dependsOn(ast)
  .dependsOn(utils)
  .dependsOn(compiler % Test)
  .dependsOn(pretty % Test)
  .settings(
    name := "lamcalcj-reducer",

    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.7" % Test
  )
lazy val reducerJS = reducer.js
lazy val reducerJVM = reducer.jvm
