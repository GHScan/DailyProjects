name := "IntellijSBT"

version := "1.0"

scalaVersion := "2.11.4"

scalaSource in Compile := baseDirectory.value / "src/main"

scalaSource in Test := baseDirectory.value / "src/test"
