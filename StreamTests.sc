
import de.flwi.mp4analyzer.Mp4Analyzer

import collection.immutable._

def next(i: Int): Stream[Int] = Stream.cons(i, next(3*math.pow(2, i).toInt))

val str = next(0)

str.take(10).mkString(", ")

def make : Stream[Int] = Stream.cons(util.Random.nextInt(10), make)

val randomStream = make
randomStream(0)
randomStream(0)
randomStream(1)
randomStream(1)
randomStream(2)
randomStream(2)
randomStream(3)
randomStream(3)

def terminatingStream(i:Int) : Stream[String] = {
if(i==0) 0.toString #:: Stream.empty
else Stream.cons(i.toString, terminatingStream(i-1))
}
val finite = terminatingStream(10)
finite.mkString(", ")


