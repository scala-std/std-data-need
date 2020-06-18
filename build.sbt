organization := Settings.Organization
name         := "std-data-need"
version      := Settings.Version
licenses     += ("MIT", url("http://opensource.org/licenses/MIT"))

Settings.standalone := true
resolvers ++= {
  if (Settings.standalone.value) List(Resolver.mavenLocal)
  else Nil
}
//libraryDependencies ++= {
//  if (Settings.standalone.value) List[ModuleID](
//    Settings.Organization %% "std-macro-illtyped" % Settings.Version,
//    Settings.Organization %% "std-data-iso"       % Settings.Version,
//    Settings.Organization %% "std-quantified"     % Settings.Version)
//  else Nil
//}

libraryDependencies ++= List(
  Dependencies.scalacheck % Test,
  Dependencies.scalaTest  % Test,
  Dependencies.macroCompat,
  scalaOrganization.value % "scala-compiler" % scalaVersion.value % Provided
)

// @silence annotation
libraryDependencies in ThisBuild ++= Seq(
  compilerPlugin(Dependencies.Plugin.silencerPlugin),
  Dependencies.Plugin.silencer)