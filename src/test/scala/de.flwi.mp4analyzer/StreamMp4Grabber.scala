package de.flwi.mp4analyzer

import java.awt.image.BufferedImage

import akka.actor.ActorSystem
import akka.stream.{Outlet, ActorFlowMaterializer}
import akka.stream.scaladsl.{FlowGraph, Source}

class StreamMp4Grabber {

  implicit val system = ActorSystem("reactive-frames")
  implicit val materzializer = ActorFlowMaterializer()


  val frames: Source[Int, Unit] = Source() { implicit b =>


    import FlowGraph.Implicits._

    val frames = Source(() => Iterator.from(1))

    val outlet: Outlet[Int] = b.add(frames)

    outlet

  }

}


