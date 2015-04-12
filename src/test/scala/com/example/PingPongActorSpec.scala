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

      val tuples: Stream[(Mat, BufferedImage)] = Stream
        .from(1)
        .map { frameNumber =>
          val grabFrame: Frame = g.grabFrame()
          val mat: Mat = x.convert(grabFrame)
          val bufferedImage = Try(converter.convert(grabFrame)).toOption

          (mat, bufferedImage)
      }
        .filter(_._2.isDefined)
        .map(tuple => tuple.copy(_2 = tuple._2.get))
        .take(limit)

      g.stop()


      tuples.zipWithIndex.foreach { case ((mat, image), index) =>
          println(s"written image $index")
          ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threesmovement/${index.formatted(formatString)}.png"))
      }

    }
  }

}
