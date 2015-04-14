package de.flwi.mp4analyzer

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import org.bytedeco.javacv.OpenCVFrameConverter.ToMat
import org.bytedeco.javacv.{Frame, Java2DFrameConverter, FFmpegFrameGrabber}

import scala.collection.mutable
import scala.util.Try

object Mp4Analyzer {

  val frameOffset = 7

  def getMatchAreas(cardSize: Size): List[MatchArea] = {

    val fromTop = Rectangle(Point(83,141),Size(375,13))
    val fromBottom = Rectangle(Point(83,664),Size(375,13))

    val fromLeft = Rectangle(Point(58,157),Size(13,502))
    val fromRight = Rectangle(Point(469,157),Size(13,502))

    val topRectangles = splitHorizontalRectangleIntoCardWindows(fromTop, cardSize)
    val bottomRectangles = splitHorizontalRectangleIntoCardWindows(fromBottom, cardSize)
    val leftRectangles = splitVerticalRectangleIntoCardWindows(fromLeft, cardSize)
    val rightRectangles = splitVerticalRectangleIntoCardWindows(fromRight, cardSize)

    val directions = (topRectangles, "top") ::(bottomRectangles, "bottom") ::(leftRectangles, "left") ::(rightRectangles, "right") :: Nil

    for ((rectangles, direction) <- directions;
         (rect, index) <- rectangles.zipWithIndex)
      yield MatchArea(rect, index, direction)
  }

  def calcColorsOfMatchArea(matchArea: MatchArea, image: BufferedImage): Vector[Color] = {
    matchArea.area.coordinates.map(coord => new Color(image.getRGB(coord.x, coord.y)))
  }

  def getColorInformationOfMatchAreas(matchAreas: List[MatchArea], image: BufferedImage): Map[MatchArea, Vector[Color]] = {
    matchAreas.map(ma => (ma, calcColorsOfMatchArea(ma, image))).toMap
  }

  def analyze(videoFilePath: String, outputFolderPath: String): Map[Int, MatchArea] = {

    val g = new FFmpegFrameGrabber(videoFilePath)

    //      val g = new OpenCVFrameGrabber(videoFilePath)
    //      g.start()

    val converter = new Java2DFrameConverter()

    val x = new ToMat

    g.start()

    val numberOfFrames = g.getLengthInFrames

    val numberOfDigits = numberOfFrames.toString.length
    val formatString = s"%0${numberOfDigits}d"

    val cardSize: Size = Size(64, 114)
    val matchAreas = getMatchAreas(cardSize)

    var firstFrame: Option[BufferedImage] = None
    var firstFramesReferenceAreas: Map[MatchArea, Vector[Color]] = Map.empty

    var lastDiffedImage: Option[BufferedImage] = None

    case class FrameNumberChangeDetected(frameNumber: Int, matchArea: MatchArea)

    var grabFrame: Option[Frame] = None
    var frameNumber = 0
    var numberOfGrabs = 0

    val framesWithChangeDetected = mutable.HashMap.empty[Int, MatchArea]

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
        val maybeFrameToGrab = framesWithChangeDetected.get(frameNumber - frameOffset)
        if (frameNumber == 1 || maybeFrameToGrab.isDefined) {
          val filenameAddition = if (frameNumber > 1) s"-from-${maybeFrameToGrab.get.direction}-${maybeFrameToGrab.get.index}" else ""
          val filename: String = s"${frameNumber.formatted(formatString)}-newBoard" + filenameAddition
          ImageIO.write(image, "png", new File(s"$outputFolderPath/$filename.png"))
        }

        if (lastDiffedImage.isEmpty) {
          println(s"#$frameNumber set reference image")
          lastDiffedImage = Some(image)

          //store reference color if not already set
          if (firstFrame.isEmpty) {
            firstFrame = Some(image)
            firstFramesReferenceAreas = getColorInformationOfMatchAreas(matchAreas, image)
          }
        } else {

          val maybeArea = findAreaWithChangedPixelColors(frameNumber, firstFramesReferenceAreas, image)
          maybeArea.foreach { ma =>
            //check, if last movement detection was 1-2 frames ago - then remove the old detection to avoid double scan

            val lastFrameWithMovement: Int = if(framesWithChangeDetected.keySet.isEmpty) 0 else framesWithChangeDetected.keySet.max
            val skipThisDetection = lastFrameWithMovement + 2 >= frameNumber
            if (skipThisDetection && framesWithChangeDetected.contains(lastFrameWithMovement)) {
              println(s"duplicate-detection in #$frameNumber (max: $lastFrameWithMovement)")
              framesWithChangeDetected -= lastFrameWithMovement
            } else {
              //ImageIO.write(image, "png", new File(s"$outputFolderPath/movement-detected-${frameNumber.formatted(formatString)}-match-from-${ma.direction}-${ma.index}.png"))
            }
            framesWithChangeDetected += frameNumber -> ma
          }
        }
      }
    }
    g.stop()

    framesWithChangeDetected.toMap
  }

  def findAreaWithChangedPixelColors(frameNumber: Int, firstFramesReferenceAreas: Map[MatchArea, Vector[Color]], image: BufferedImage): Option[MatchArea] = {

    val matchAreas = firstFramesReferenceAreas.keySet
    val maybeArea = matchAreas.find { ma =>

      val colors = ma.area.coordinates.map(coord => new Color(image.getRGB(coord.x, coord.y)))
      val colorsVsReferenceColors = colors.zip(firstFramesReferenceAreas.get(ma).get)
      val colorDistances = colorsVsReferenceColors.map { case (cThis, cReference) => calcColorDistance(cThis, cReference) }
      val numberOfPixelsChanged: Int = colorDistances.count(_ > 25)

      val significantChangeDetected = numberOfPixelsChanged > ma.area.coordinates.size * 0.1

//      if (numberOfPixelsChanged > 0) println(s"#$frameNumber; numberOfPixelsChanged: $numberOfPixelsChanged; direction: ${ma.direction}; Index: ${ma.index}")
//
//      if (significantChangeDetected) {
//        println(s"#$frameNumber found match in $frameNumber; from-matchArea: ${ma.direction}; Index: ${ma.index}")
//      }
      significantChangeDetected
    }
    maybeArea
  }

  def splitHorizontalRectangleIntoCardWindows(rectangle: Rectangle, cardSize: Size): List[Rectangle] = {
    val offset = (rectangle.size.width - 4 * cardSize.width) / 5

    0.until(4).map { index =>
      val topLeft: Point = rectangle.topLeft.copy(x = rectangle.topLeft.x + offset + index * (cardSize.width + offset))
      Rectangle(topLeft, Size(cardSize.width, rectangle.size.height))
    }.toList
  }

  def splitVerticalRectangleIntoCardWindows(rectangle: Rectangle, cardSize: Size): List[Rectangle] = {
    val offset = (rectangle.size.height - 4 * cardSize.height) / 5

    0.until(4).map { index =>
      val topLeft: Point = rectangle.topLeft.copy(y = rectangle.topLeft.y + offset + index * (cardSize.height + offset))
      Rectangle(topLeft, Size(rectangle.size.width, cardSize.height))
    }.toList
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

}
