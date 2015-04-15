package de.flwi.mp4analyzer

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

import org.bytedeco.javacv.{FFmpegFrameGrabber, Java2DFrameConverter}

import scala.collection.immutable.Stream
import scala.collection.mutable
import scala.util.Try

object Mp4Analyzer {

  val frameOffset = 7

  def getMatchAreas(cardSize: Size): List[MatchArea] = {

    val fromTop = Rectangle(Point(83, 141), Size(375, 13))
    val fromBottom = Rectangle(Point(83, 664), Size(375, 13))

    val fromLeft = Rectangle(Point(58, 157), Size(13, 502))
    val fromRight = Rectangle(Point(469, 157), Size(13, 502))

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

  def getFrameStream(videoFilePath: String): Stream[(Int, BufferedImage)] = {
    val converter = new Java2DFrameConverter()

    def terminatingStream(g: FFmpegFrameGrabber): Stream[(Int, BufferedImage)] = {
      val grabFrame = Option(g.grabFrame(true))
      val frameNumber: Int = g.getFrameNumber
      println(s"Frame: #$frameNumber, grabFrame: $grabFrame")

      grabFrame match {
        //No frame could be grabbed - just stop
        case None =>
          g.stop()
          Stream.empty
        case Some(frame) =>
          //it is possible, that a frame can't be converted to an image.
          //So just pick the next one and try again
          val maybeImage = Try(converter.convert(frame)).toOption
          maybeImage match {
            case None => terminatingStream(g)
            case Some(image) => Stream.cons((frameNumber, image), terminatingStream(g))
          }
      }
    }

    val g: FFmpegFrameGrabber = new FFmpegFrameGrabber(videoFilePath)
    g.start()

    terminatingStream(g)
  }

  def getNewBoardsStream(framesStream: Stream[(Int, BufferedImage)], matchAreas: List[MatchArea]): Stream[(Int, BufferedImage, MatchArea)] = {

    var mayBeFirstFramesReferenceAreas = Option.empty[Map[MatchArea, Vector[Color]]]

    def helper(streamOfFrames: Stream[(Int, BufferedImage)], framesWithMovement: Map[Int, MatchArea]): Stream[(Int, BufferedImage, MatchArea)] = {

      streamOfFrames match {
        case (frameNumber, image) #:: cons if framesWithMovement.contains(frameNumber - 7) =>
          (frameNumber, image, framesWithMovement(frameNumber - 7)) #:: helper(cons, framesWithMovement)

        case (frameNumber, image) #:: cons =>
          //grab the first frame as reference
          if (frameNumber == 0) mayBeFirstFramesReferenceAreas = Some(getColorInformationOfMatchAreas(matchAreas, image))

          //analyze frame
          val maybeMatchArea: Option[MatchArea] = mayBeFirstFramesReferenceAreas match {
            case None => None
            case Some(firstFramesReferenceAreas) => findAreaWithChangedPixelColors(frameNumber, firstFramesReferenceAreas, image)
          }

          //check for movement
          maybeMatchArea match {
            case None => helper(cons, framesWithMovement)
            case Some(area) =>

              //has another frame been found that was very close to this frame? if so, remove the old find and add this one
              val lastFrameWithMovement: Int = if (framesWithMovement.keySet.isEmpty) 0 else framesWithMovement.keySet.max
              val useThisDetectionInsteadOfLastOne = lastFrameWithMovement + 2 >= frameNumber
              if (useThisDetectionInsteadOfLastOne) framesWithMovement.-(frameNumber)

              helper(cons, framesWithMovement.updated(frameNumber, area))
          }
        case _ => Stream.empty
      }
    }

    helper(framesStream, Map.empty)
  }

  def analyzeAndWriteToImageFiles(videoFilePath: String, outputFolderPath: String): Map[Int, MatchArea] = {

    getNewBoardsStream(getFrameStream(videoFilePath), getMatchAreas(Size()))

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
