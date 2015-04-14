package com.example

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{FFmpegFrameGrabber, Frame, Java2DFrameConverter}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.util.Try


case class Point(x: Int, y: Int)

case class Size(width: Int, height: Int)

object Rectangle {
  def apply(start: Point, end: Point): Rectangle = {
    val size = Size(end.x - start.x, end.y - start.y)
    Rectangle(start, size)
  }
}

case class Rectangle(topLeft: Point, size: Size) {

  val coordinates = (for (y <- 0.until(size.height); x <- 0.until(size.width)) yield Point(topLeft.x + x, topLeft.y + y)).toVector
}

case class MatchArea(area: Rectangle, index: Int, direction: String)

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

      val numberOfDigits = numberOfFrames.toString.length
      val formatString = s"%0${numberOfDigits}d"


      val fromLeftCoordinates = (for (x <- 62.to(75);
                                      y <- 183.to(638))
        yield Point(x, y)).toList

      val fromRightCoordinates = (for (x <- 462.to(475);
                                       y <- 183.to(638))
        yield Point(x, y)).toList

      val fromTopCoordinates = (for (x <- 98.to(440);
                                     y <- 143.to(154))
        yield Point(x, y)).toList

      val fromBottomCoordinates = (for (x <- 98.to(440);
                                        y <- 667.to(680))
        yield Point(x, y)).toList

      case class CoordinatesAndDirection(coordinates: List[Point], direction: String)

      val coordinates = List(
        CoordinatesAndDirection(fromLeftCoordinates, "left"),
        CoordinatesAndDirection(fromRightCoordinates, "right"),
        CoordinatesAndDirection(fromTopCoordinates, "top"),
        CoordinatesAndDirection(fromBottomCoordinates, "bottom")
      )

      val cardSize = Size(64, 114)

      val fromTop = Rectangle(Point(83, 143), Point(458, 156))
      val fromBottom = Rectangle(Point(83, 662), Point(458, 675))

      val fromLeft = Rectangle(Point(20, 157), Point(75, 659))
      val fromRight = Rectangle(Point(459, 157), Point(472, 659))


      def splitHorizontalRectangleIntoCardWindows(rectangle: Rectangle, cardSize: Size): List[Rectangle] = {
        val offset = (rectangle.size.width - 4 * cardSize.width) / 5

        0.until(4).map { index =>
          val topLeft: Point = rectangle.topLeft.copy(x = offset + index * (cardSize.width + offset))
          Rectangle(topLeft, Size(cardSize.width, rectangle.size.height))
        }.toList
      }

      def splitVerticalRectangleIntoCardWindows(rectangle: Rectangle, cardSize: Size): List[Rectangle] = {
        val offset = (rectangle.size.height - 4 * cardSize.height) / 5

        0.until(4).map { index =>
          val topLeft: Point = rectangle.topLeft.copy(y = offset + index * (cardSize.height + offset))
          Rectangle(topLeft, Size(rectangle.size.width, cardSize.height))
        }.toList
      }


      val topRectangles = splitHorizontalRectangleIntoCardWindows(fromTop, cardSize)
      val bottomRectangles = splitHorizontalRectangleIntoCardWindows(fromBottom, cardSize)
      val leftRectangles = splitVerticalRectangleIntoCardWindows(fromLeft, cardSize)
      val rightRectangles = splitVerticalRectangleIntoCardWindows(fromRight, cardSize)

      val directions = (topRectangles, "top") ::(bottomRectangles, "bottom") ::(leftRectangles, "left") ::(rightRectangles, "right") :: Nil



      val matchAreas: List[MatchArea] = for ((rectangles, direction) <- directions;
                                             (rect, index) <- rectangles.zipWithIndex
      )
        yield MatchArea(rect, index, direction)


      var firstFrame: Option[BufferedImage] = None
      var firstFramesReferenceAreas: Map[MatchArea, Vector[Color]] = Map.empty

      var lastDiffedImage: Option[BufferedImage] = None
      var lastFrameWithColorChange: Option[FrameNumberChangeDetected] = None

      case class FrameNumberChangeDetected(frameNumber: Int, matchArea: MatchArea)

      var grabFrame: Option[Frame] = None
      var frameNumber = 0
      var numberOfGrabs = 0
      while ( {
        grabFrame = Option(g.grabFrame(true))
        numberOfGrabs += 1
        grabFrame.isDefined
      }
      ) {

        val bufferedImage = Try(converter.convert(grabFrame.get)).toOption

        bufferedImage.foreach { image =>
          frameNumber = g.getFrameNumber
          println(s"analyzing frame #$frameNumber (of $numberOfGrabs grabs)")

          //check, if this image is n frames after the matchedFrame
          if (frameNumber == 1 || lastFrameWithColorChange.isDefined && frameNumber >= lastFrameWithColorChange.get.frameNumber + 7) {
            val filenameAddition = if(frameNumber > 1) s"-from-${lastFrameWithColorChange.get.matchArea.direction}-${lastFrameWithColorChange.get.matchArea.index}" else ""
            val filename: String = s"${frameNumber.formatted(formatString)}-newBoard" + filenameAddition
            ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threes-capture/$filename.png"))
            lastFrameWithColorChange = None
          }

          if (lastDiffedImage.isEmpty) {
            println(s"#$frameNumber set reference image")
            lastDiffedImage = Some(image)

            //store reference color if not already set
            if (firstFrame.isEmpty) {
              firstFrame = Some(image)
              firstFramesReferenceAreas = matchAreas.map(ma => ma -> ma.area.coordinates.map(coord => new Color(image.getRGB(coord.x, coord.y)))).toMap
            }
          } else {


              val maybeArea = matchAreas.find { ma =>

                val colors = ma.area.coordinates.map(coord => new Color(image.getRGB(coord.x, coord.y)))
                val colorsVsReferenceColors = colors.zip(firstFramesReferenceAreas.get(ma).get)
                val colorDistances = colorsVsReferenceColors.map { case (cThis, cReference) => calcColorDistance(cThis, cReference) }
                val numberOfPixelsChanged: Int = colorDistances.count (_ > 25 )

                val significantChangeDetected = numberOfPixelsChanged > ma.area.coordinates.size * 0.1

                if(numberOfPixelsChanged > 0) println(s"#$frameNumber; numberOfPixelsChanged: $numberOfPixelsChanged; direction: ${ma.direction}; Index: ${ma.index}")


                if (significantChangeDetected) {

                  println(s"#$frameNumber found match in $frameNumber; from-matchArea: ${ma.direction}; Index: ${ma.index}")
                  //ImageIO.write(image, "png", new File(s"/home/flwi/Pictures/threes-capture/${frameNumber.formatted(formatString)}-match-from-${ma.direction}-${ma.index}.png"))

                }

                significantChangeDetected
              }

              maybeArea.foreach { ma =>
                lastFrameWithColorChange = Some(FrameNumberChangeDetected(frameNumber, ma))
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