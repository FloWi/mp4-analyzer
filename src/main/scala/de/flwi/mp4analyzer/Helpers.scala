package de.flwi.mp4analyzer

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
