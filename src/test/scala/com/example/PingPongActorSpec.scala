package com.example

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{FFmpegFrameGrabber, Java2DFrameConverter}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

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

      var lastDiffedImage: Option[BufferedImage] = None
      var lastMatchedFrame = 0


      1.to(numberOfFrames).foreach { frameNumber =>

        val bufferedImage = Try(converter.convert(g.grabFrame(true))).toOption

        bufferedImage.foreach { image =>

          if (lastDiffedImage.isEmpty) {
            println("set reference image")
            lastDiffedImage = Some(image)

            //store reference color if not already set
            if (referenceColor.isEmpty) {
              referenceColor = Some(image.getRGB(12, 187))
            }
          } else {

            if (!bufferedImagesEqual(lastDiffedImage.get, image)) {
              println("found diffing image, analyzing ...")
              lastDiffedImage = Some(image)

              //check, if this image is n frames after the matchedFrame
              if (frameNumber == lastMatchedFrame + 3) {
                ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threes-capture/${frameNumber.formatted(formatString)}-newBoard.png"))
              }

              val colors = coordinates.map(coord => image.getRGB(coord.x, coord.y))
              val allSameAsReference = colors.forall(c => c == referenceColor.get)

              if (!allSameAsReference) {
                println(s"found match in $frameNumber")
                println("colors: " + colors.groupBy(c => c).map(tup => s"${tup._1}: ${tup._2.size}x"))
                lastMatchedFrame = frameNumber
                ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threes-capture/${frameNumber.formatted(formatString)}-match.png"))
              }
            }
          }
        }
      }

      g.stop()
    }
  }

  def bufferedImagesEqual(img1: BufferedImage, img2: BufferedImage): Boolean = {

    def getColors(img: BufferedImage) = for (x <- 0.until(img.getWidth);
                                             y <- 0.until(img.getHeight))
      yield new Color(img.getRGB(x, y))


    def calcColorDistance(c1: Color, c2: Color): Double = {
      val rmean: Double = (c1.getRed + c2.getRed) / 2
      val r: Int = c1.getRed - c2.getRed
      val g: Int = c1.getGreen - c2.getGreen
      val b: Int = c1.getBlue - c2.getBlue
      val weightR: Double = 2 + rmean / 256
      val weightG: Double = 4.0
      val weightB: Double = 2 + (255 - rmean) / 256
      math.sqrt(weightR * r * r + weightG * g * g + weightB * b * b)
    }

    if (img1.getWidth != img2.getWidth || img1.getHeight != img2.getHeight) {
      false
    } else {

      val colors1 = getColors(img1)
      val colors2 = getColors(img2)

      val diffingPixels: List[((Color, Color, Double), Int)] = colors1
        .zip(colors2)
        //.map{ case (c1, c2) => c1.getRGB-c2.getRGB }
        .zipWithIndex
        .map { case ((c1, c2), index) => ((c1, c2, calcColorDistance(c1, c2)), index) }
        .filter { case ((_, _, diff), _) => diff > 100 }.toList

      if (diffingPixels.size < 100) {

        true
      } else {
        //println(s"${diffingPixels.size} pixels differ")
        false
      }
    }
  }


}