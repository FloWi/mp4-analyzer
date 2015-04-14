package de.flwi.mp4analyzer

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.{FileFilter, File}
import java.net.URL
import javax.imageio.ImageIO

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}


class MovieGrabberSpec(_system: ActorSystem) extends TestKit(_system) with ImplicitSender
with WordSpecLike with Matchers with BeforeAndAfterAll {

  def this() = this(ActorSystem("MySpec"))

  override def afterAll() {
    TestKit.shutdownActorSystem(system)
  }

  "A movie" must {
    "be deconstructed in frames properly" in {

      //val videoFilePath: String = "/home/flwi/Downloads/Short video clip-nature.mp4-SD.mp4"
      val videoFilePath: String = "/home/flwi/ownCloud/douglas/82k_threes.mp4"

      val framesWithMovement: Map[Int, MatchArea] = Mp4Analyzer.analyze(videoFilePath, "/home/flwi/Pictures/threes-capture/secondRun/")

      val frameNumbersWithMovement = framesWithMovement.keys.toList.sorted.mkString(", ")
      val frameNumbersToGrab = framesWithMovement.keys.toList.map(_+Mp4Analyzer.frameOffset).sorted.mkString(", ")

      println(
        s"""Frames with movement
           |$frameNumbersWithMovement""".stripMargin)

      println(
        s"""Frames to grab
           |$frameNumbersToGrab""".stripMargin)
    }
  }

  "Images with a card coming in" must {
    "be detected correctly" in {

      val referenceImage = ImageIO.read(new File(getClass.getResource("/newBoard.png").getFile))
      val matchAreas: List[MatchArea] = Mp4Analyzer.getMatchAreas(Size(64, 114))
      
      val referenceImageInformation = Mp4Analyzer.getColorInformationOfMatchAreas(matchAreas, referenceImage)

      val url: URL = getClass.getResource("/not_matched/")
      val imageFolder = new File(url.getFile)
      val imageFiles: List[File] = imageFolder.listFiles().toList

      val images = imageFiles.map(imageFile => (imageFile.getName, ImageIO.read(imageFile))).toMap

      images.forall{ case (_, image) =>
        Mp4Analyzer.findAreaWithChangedPixelColors(0, referenceImageInformation, image).isDefined
      }

      val fromLeft3WithWhiteCard = images("image-237.png")
      val maybeMatchArea: Option[MatchArea] = Mp4Analyzer.findAreaWithChangedPixelColors(0, referenceImageInformation, fromLeft3WithWhiteCard)
      assert(maybeMatchArea.isDefined)
      assert(maybeMatchArea.get.index == 3)
      assert(maybeMatchArea.get.direction == "left")

    }
  }

  "Visually debugging" must {
    "show that the right areas are being selected" in {

      val filename: String = "/boardWithHighTile.png"

      val referenceImage = ImageIO.read(new File(getClass.getResource(filename).getFile))
      val matchAreas: List[MatchArea] = Mp4Analyzer.getMatchAreas(Size(64, 114))

      val referenceImageInformation = Mp4Analyzer.getColorInformationOfMatchAreas(matchAreas, referenceImage)

      val allMatchAreasImage = ImageIO.read(new File(getClass.getResource(filename).getFile))

      val coloredMatchAreas: Map[MatchArea, BufferedImage] = referenceImageInformation.map { case (ma, _) =>
        val imageForThisMa = ImageIO.read(new File(getClass.getResource(filename).getFile))
        ma.area.coordinates.foreach(coord => allMatchAreasImage.setRGB(coord.x, coord.y, Color.gray.getRGB))
        ma.area.coordinates.foreach(coord => imageForThisMa.setRGB(coord.x, coord.y, Color.gray.getRGB))
        ImageIO.write(allMatchAreasImage, "png", new File(s"/tmp/matchArea-from-${ma.direction}-index-${ma.index}.png"))
        (ma, imageForThisMa)
      }

      ImageIO.write(allMatchAreasImage, "png", new File(s"/tmp/all-matchAreas.png"))


      coloredMatchAreas

    }
  }
}