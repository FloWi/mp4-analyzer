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
      val fromLeftCoordinates = (for (x <- 62.to(75);
                                      y <- 183.to(638))
        yield Coordinate(x, y)).toList

      val fromRightCoordinates = (for (x <- 462.to(475);
                                       y <- 183.to(638))
        yield Coordinate(x, y)).toList

      val fromTopCoordinates = (for (x <- 98.to(440);
                                     y <- 143.to(154))
        yield Coordinate(x, y)).toList

      val fromBottomCoordinates = (for (x <- 98.to(440);
                                        y <- 667.to(680))
        yield Coordinate(x, y)).toList

      case class CoordinatesAndDirection(coordinates: List[Coordinate], direction: String)

      val coordinates = List(
        CoordinatesAndDirection(fromLeftCoordinates, "left"),
        CoordinatesAndDirection(fromRightCoordinates, "right"),
        CoordinatesAndDirection(fromTopCoordinates, "top"),
        CoordinatesAndDirection(fromBottomCoordinates, "bottom")
      )


      var referenceColor: Option[Color] = None

      var lastDiffedImage: Option[BufferedImage] = None
      var lastFrameWithColorChange: Option[FrameNumberChangeDetected] = None

      case class FrameNumberChangeDetected(frameNumber: Int, direction: String)

      1.to(numberOfFrames).foreach { frameNumber =>

        val bufferedImage = Try(converter.convert(g.grabFrame(true))).toOption

        bufferedImage.foreach { image =>

          //check, if this image is n frames after the matchedFrame
          if (lastFrameWithColorChange.isDefined && frameNumber >= lastFrameWithColorChange.get.frameNumber + 15) {
            ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threes-capture/${frameNumber.formatted(formatString)}-newBoard-from-${lastFrameWithColorChange.get.direction}.png"))
            lastFrameWithColorChange = None
          }

          if (lastDiffedImage.isEmpty) {
            println(s"#$frameNumber set reference image")
            lastDiffedImage = Some(image)

            //store reference color if not already set
            if (referenceColor.isEmpty) {
              referenceColor = Some(new Color(image.getRGB(12, 187)))
            }
          } else {

            //check for change
            if (!bufferedImagesEqual(lastDiffedImage.get, image)) {
              println(s"#$frameNumber found diffing image, analyzing. further...")
              lastDiffedImage = Some(image)

              coordinates.foreach { case CoordinatesAndDirection(coords, direction) =>

                val colors = coords.map(coord => new Color(image.getRGB(coord.x, coord.y)))
                val numberOfPixelsChanged: Int = colors.count(c => calcColorDistance(c, referenceColor.get) > 100)

                val significantChangeDetected = numberOfPixelsChanged > coords.size * 0.10

                if (significantChangeDetected) {
                  lastFrameWithColorChange = Some(FrameNumberChangeDetected(frameNumber, direction))

                  println(s"#$frameNumber found match in $frameNumber")
                  println(s"#$frameNumber colors: " + colors.groupBy(c => c).map(tup => s"${tup._1}: ${tup._2.size}x"))
                  ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threes-capture/${frameNumber.formatted(formatString)}-match-from-$direction.png"))

                }
              }
            }
          }
        }
      }

      g.stop()
    }
  }

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

  def bufferedImagesEqual(img1: BufferedImage, img2: BufferedImage): Boolean = {

    def getColors(img: BufferedImage) = for (x <- 0.until(img.getWidth);
                                             y <- 0.until(img.getHeight))
      yield new Color(img.getRGB(x, y))



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
        .filter { case ((_, _, diff), _) => diff > 120 }.toList

      if (diffingPixels.size < 100) {

        true
      } else {
        //println(s"${diffingPixels.size} pixels differ")
        false
      }
    }
  }


}