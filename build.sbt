name := "scala-regexp-play"
    
version := "1.0"
    
scalaVersion := "2.11.8"

scalacOptions := Seq("-feature", "-unchecked", "-deprecation", "-encoding", "utf8", "-Xlint:_", "-Ywarn-unused-import")
  
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.6"

libraryDependencies += "org.scalaz" %% "scalaz-concurrent" % "7.2.6"