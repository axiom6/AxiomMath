
name := Settings.Name

unmanagedJars in Compile += file("jar/Jama-1.0.3.jar")

//scalacOptions += "-language:implicitConversions" // instead use import  scala.language.implicitConversions

libraryDependencies ++= Dependencies.AxiomMath

fork in run := true
