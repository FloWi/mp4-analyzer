package de.flwi.mp4analyzer

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{FFmpegFrameGrabber, Frame, Java2DFrameConverter}
import org.scalatest.{WordSpecLike, Matchers, BeforeAndAfterAll}

import scala.util.Try


class MovieGrabberSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("MySpec"))

  override def afterAll {
    TestKit.shutdownActorSystem(system)
  }

  "A movie" must {
    "be deconstructed in frames properly" in {

      //val videoFilePath: String = "/home/flwi/Downloads/Short video clip-nature.mp4-SD.mp4"
      val videoFilePath: String = "/home/flwi/ownCloud/douglas/82k_threes.mp4"

    }
  }
}