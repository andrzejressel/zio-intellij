lazy val scala212      = "2.12.10"
lazy val pluginVersion = "2020.2.1.3" + sys.env.get("ZIO_INTELLIJ_BUILD_NUMBER").fold("")("." + _)

ThisBuild / intellijPluginName := "zio-intellij"
ThisBuild / intellijBuild := "202"

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

lazy val `zio-intellij` = project
  .in(file("."))
  .enablePlugins(SbtIdeaPlugin)
  .settings(
    scalaVersion := scala212,
    version := pluginVersion,
    intellijPlugins := Seq(
      "com.intellij.java".toPlugin,
      "org.intellij.scala:2020.2.27".toPlugin
    ),
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % Test,
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v", "-s", "-a", "+c", "+q"),
    patchPluginXml := pluginXmlOptions { xml =>
      xml.version = version.value
      xml.changeNotes = sys.env.getOrElse(
        "ZIO_INTELLIJ_CHANGE_NOTES",
        s"""<![CDATA[
        Welcome to another exciting release of the ZIO IntelliJ Plugin! Huge thanks to all the contributors who made this release possible!<br/>
        Here are the highlights:
        <ul>
        <li>Add simplification from <code>layer.build.use</code> to <code>zio.provideLayer</code> (<a href="https://github.com/zio/zio-intellij/pull/172">#172</a>)</li>
        <li>Miscellaneous bug fixes</li>
        <ul>
        ]]>"""
      )
    }
  )

lazy val runner = createRunnerProject(`zio-intellij`)
