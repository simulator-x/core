scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "lib") ** "*.jar").classpath }

unmanagedJars in Compile <<= baseDirectory map { base => ((base ** "assets") ** "*.jar").classpath }

autoCompilerPlugins := true

classDirectory in Compile <<= target(_ / "scala/classes")

classDirectory in Test <<= target(_ / "scala/test-classes")

ivyXML := scala.xml.XML.load( core.base + "/ivy.xml" ) \ "dependencies"

libraryDependencies <<= (scalaVersion, libraryDependencies) { (ver, deps) =>
    deps :+ compilerPlugin("org.scala-lang.plugins" % "scala-continuations-plugin_2.11.2" % "1.0.2")
}

scalacOptions += "-P:continuations:enable"

scalacOptions += "-deprecation"
 
scalacOptions += "-feature"

scalacOptions += "-language:postfixOps"

scalacOptions += "-language:existentials"

scalacOptions += "-language:implicitConversions"
