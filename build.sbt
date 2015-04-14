name := """mp4analyzer"""

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.9",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.9" % "test",
  "com.typesafe.akka" % "akka-stream-experimental_2.11" % "1.0-M5",
  "org.scalatest" %% "scalatest" % "2.2.4" % "test")


classpathTypes += "maven-plugin"

libraryDependencies += "org.bytedeco" % "javacv" % "0.11"

libraryDependencies += "org.bytedeco" % "javacpp" % "0.11"

libraryDependencies += "org.bytedeco" % "javacpp-presets" % "0.11"
//
//libraryDependencies += "org.bytedeco.javacpp-presets" % "artoolkitplus" % "2.3.1-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "fftw" % "3.3.4-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "videoinput" % "0.200-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "flycapture" % "2.7.3.19-0.11" classifier "linux-x86_64"
libraryDependencies += "org.bytedeco.javacpp-presets" % "opencv" % "2.4.11-0.11" classifier "" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "libfreenect" % "0.5.2-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "tesseract" % "3.03-rc1-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "llvm" % "3.6.0-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "flandmark" % "1.07-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "ffmpeg" % "2.6.1-0.11"
libraryDependencies += "org.bytedeco.javacpp-presets" % "ffmpeg" % "2.6.1-0.11" classifier "" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "gsl" % "1.16-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "libdc1394" % "2.2.3-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "leptonica" % "1.71-0.11" classifier "linux-x86_64"
//libraryDependencies += "org.bytedeco.javacpp-presets" % "caffe" % "master-0.11" classifier "linux-x86_64"