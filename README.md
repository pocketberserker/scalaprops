# scalaprops on Scala.JS

[![Build Status](https://travis-ci.org/pocketberserker/scalaprops.svg?branch=develop)](https://travis-ci.org/pocketberserker/scalaprops)

This has been compiled for Scala.JS and published maven central under `com.github.pocketberserker`.

### latest stable version

```scala
testFrameworks += new TestFramework("scalaprops.ScalapropsFramework")

parallelExecution in Test := false // currently, does not support parallel execution

libraryDependencies += "com.github.pocketberserker" %%% "scalaprops" % "0.1.13" % "test"
```

```scala
libraryDependencies += "com.github.pocketberserker" %%% "scalaprops-scalazlaws" % "0.1.13" % "test"
```

