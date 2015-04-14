package de.flwi.mp4analyzer

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

      Mp4Analyzer.analyze(videoFilePath)
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
        image.getHeight == 720 && image.getWidth == 540
      }

      val fromLeft3WithWhiteCard = images("image-237.png")
      val maybeMatchArea: Option[MatchArea] = Mp4Analyzer.findAreaWithChangedPixelColors(0, referenceImageInformation, fromLeft3WithWhiteCard)
      assert(maybeMatchArea.isDefined)
      assert(maybeMatchArea.get.index == 3)
      assert(maybeMatchArea.get.direction == "left")



    }
  }
}