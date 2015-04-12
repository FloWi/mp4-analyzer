package com.example

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.bytedeco.javacpp.opencv_core.{IplImage, Mat}
import org.bytedeco.javacpp.opencv_highgui.CvCapture
import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{OpenCVFrameGrabber, Frame, FFmpegFrameGrabber, Java2DFrameConverter}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.collection.immutable.IndexedSeq
import scala.util.Try

class PingPongActorSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("MySpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }


  "A movie" must {
    "be deconstructed in frames properly" in {

      //val videoFilePath: String = "/home/flwi/Downloads/Short video clip-nature.mp4-SD.mp4"
      val videoFilePath: String = "/home/flwi/ownCloud/douglas/82k_threes.mp4"

      val g = new FFmpegFrameGrabber(videoFilePath)

      //      val g = new OpenCVFrameGrabber(videoFilePath)
      //      g.start()

      val converter = new Java2DFrameConverter()

      val x = new ToMat

      g.start()
      val numberOfFrames = g.getLengthInFrames

      val limit: Int = 10000
      val numberOfDigits = limit.toString.length
      val formatString = s"%0${numberOfDigits}d"


      case class Coordinate(x: Int, y: Int)
      val coordinates = for (x <- 12.to(67);
      y <- 187.to(642))
      yield Coordinate(x, y)

      var referenceColor: Option[Int] = None

      1.to(numberOfFrames).foreach { framenumber =>

        val grabFrame: Frame = g.grabFrame()

        val bufferedImage = Try(converter.convert(grabFrame)).toOption

        bufferedImage.foreach { image =>

          if(referenceColor.isEmpty) {
            referenceColor = Some(image.getRGB(12, 187))
          }

          val colors = coordinates.map(coord => image.getRGB(coord.x, coord.y))
          val allSameAsReference = colors.forall(c => c == referenceColor.get)

          if(!allSameAsReference) {
            println(s"found match in $framenumber")
          }
        }
      }

//      val tuples: Stream[Int] = Stream
//        .from(1)
//        .map { frameNumber =>
//
//
//
//        (bufferedImage)
//        frameNumber
//      }
//        .filter(_._2.isDefined)
//        .map(tuple => tuple.copy(_2 = tuple._2.get))
//        .take(limit)
//
//      tuples.zipWithIndex.foreach { case ((mat, image), index) =>
//          println(s"written image $index")
//          ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threesmovement/${index.formatted(formatString)}.png"))
//      }
      g.stop()
    }
  }
}