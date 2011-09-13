package com.paulsnively

import xml.Elem

import dispatch._
import org.scala_tools.time.Imports._

object Virginia extends App {
  val windowSize = 100
  val delay = 1000

  val artist = args.mkString(" ")

  val http = new Http
  val api = :/("musicbrainz.org") / "ws/2/artist/" <:< Map("User-Agent" -> "Virginia/1.0")
  val params = Map("query" -> """type:Person AND artist:""""", "limit" -> windowSize.toString)

  def artistListFromMetadata(e: Elem) = {
    (e \ "artist-list").head.asInstanceOf[Elem]
  }

  def getArtists(numWindows: Int) = {
    val result = http(api <<? params <> identity)
    val artistList = artistListFromMetadata(result)
    val artistCount = artistList.attributes("count").text.toInt
//    val maxOffset = (artistCount / windowSize - (if (artistCount % windowSize > 0) 0 else 1)) * windowSize
    val maxOffset = numWindows * windowSize

    val moreArtists = for {
      offset <- windowSize to maxOffset by windowSize
      artist <- {
        Thread.sleep(delay)
        val moreResults = http(api <<? params + ("offset" -> offset.toString) <> identity)
        artistListFromMetadata(moreResults).child
      }
    } yield artist

    result.copy(child=artistList.copy(child=artistList.child ++ moreArtists))
  }

  println(getArtists(4))
}
