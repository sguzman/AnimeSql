package com.github.sguzman.anime.planet

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.SocketTimeoutException

import com.github.sguzman.brotli.Brotli
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elementList}
import org.msgpack.core.{MessagePack, MessagePacker, MessageUnpacker}
import scalaj.http.Http

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.{Failure, Success}

object Main {
  private implicit final class PackerWrap(msg: MessagePacker) {
    def binary(binary: Array[Byte]): Unit = {
      msg.packBinaryHeader(binary.length)
      val _ = msg.addPayload(binary)
    }

    def httpMap(): Unit = {
      msg.packMapHeader(httpCache.size)
      httpCache.keysIterator.foreach{key =>
        msg.packString(key)
        msg.binary(httpCache(key))
      }

      msg.close()
    }

    def items(): Unit = {
      val list = itemCache.animeTitles.toList
      msg.packArrayHeader(list.length)
      list.foreach {a =>
        msg.packString(a.title)
        msg.packString(a.img)
        msg.packString(a.link)
        msg.packString(a.desc)
        msg.packString(a.studio)
        msg.packString(a.year)
        msg.packDouble(a.rating)
        msg.packString(a.`type`)
        msg.packArrayHeader(a.genres.size)
        a.genres.foreach(msg.packString)
      }

      msg.close()
    }
  }

  private implicit final class UnpackerWrap(msg: MessageUnpacker) {
    def binary: Array[Byte] = {
      val valueLen = msg.unpackBinaryHeader
      msg.readPayload(valueLen)
    }

    def httpMap: Map[String, Array[Byte]] = {
      val len = msg.unpackMapHeader
      def unpackMap(buffer: Map[String, Array[Byte]] = Map(), idx: Int = 0): Map[String, Array[Byte]] =
        if (idx == len) {
          buffer
        } else {
          val key = msg.unpackString
          val value = msg.binary

          unpackMap(buffer ++ Map(key -> value), idx + 1)
        }

      val output = unpackMap()
      msg.close()
      output
    }

    def items: Items = {
      val len = msg.unpackArrayHeader
      val titles = (1 to len).map{_ =>
        val title = msg.unpackString
        val img = msg.unpackString
        val link = msg.unpackString
        val desc = msg.unpackString
        val studio = msg.unpackString
        val year = msg.unpackString
        val rating = msg.unpackDouble
        val `type` = msg.unpackString
        val genreLen = msg.unpackArrayHeader
        val genres = (1 to genreLen).map(_ => msg.unpackString).toSet

        AnimeTitle(title, img, link, desc, studio, year, rating, `type`, genres)
      }

      val is = Items(mutable.Set(titles: _*))
      msg.close()
      is
    }
  }

  val httpCache: TrieMap[String, Array[Byte]] = identity {
    val file = new File("./http.msg")
    if (!file.exists) {
      scribe.info("Creating http.msg file")
      file.createNewFile()
      TrieMap()
    } else {
      scribe.info("Found http.msg file")
      val hash = MessagePack.newDefaultUnpacker(new FileInputStream(file)).httpMap
      TrieMap[String, Array[Byte]](hash.toSeq: _*)
    }
  }

  def writeHttpCache(): Unit = {
    scribe.info("Writing http.msg...")
    val file = new File("./http.msg")
    MessagePack.newDefaultPacker(new FileOutputStream(file)).httpMap()
    scribe.info("Wrote http.msg")
  }

  final case class AnimeTitle(
                                title: String,
                                img: String,
                                link: String,
                                desc: String,
                                studio: String,
                                year: String,
                                rating: Double,
                                `type`: String,
                                genres: Set[String]
                              )
  final case class Items(animeTitles: mutable.Set[AnimeTitle])

  val itemCache: Items = identity {
    val file = new File("./items.msg")
    if (!file.exists) {
      scribe.info("Creating items.msg file")
      file.createNewFile()
      Items(mutable.Set())
    } else {
      scribe.info("Found items.msg file")
      MessagePack.newDefaultUnpacker(new FileInputStream(file)).items
    }
  }

  def writeItemCache(): Unit = {
    scribe.info("Writing items.msg...")
    val file = new File("./items.msg")
    MessagePack.newDefaultPacker(new FileOutputStream(file)).items()
    scribe.info("Wrote items.msg")
  }

  Runtime.getRuntime.addShutdownHook(new Thread(() => {
    writeItemCache()
    writeHttpCache()
  }))

  def httpCacheQuery(url: String): String =
    if (httpCache.contains(url)) {
      Brotli.decompress(httpCache(url))
    } else {
      val body = Http(url).asString.body
      httpCache.put(url, Brotli.compress(body))
      body
    }

  def retryHttpGet(url: String): String = util.Try(httpCacheQuery(url)) match {
    case Success(v) => v
    case Failure(e) => e match {
      case _: SocketTimeoutException => retryHttpGet(url)
      case _ => throw new Exception(s"Url: $url; ${e.getMessage}")
    }
  }

  implicit final class DocWrap(doc: Browser#DocumentType) {
    def map(s: String, a: String = ""): Element = doc.>?>(element(s)) match {
      case Some(v) => v
      case None => throw new Exception(s"$s $a")
    }

    def maybeMap[A](s: String, f: Element => A, a: A): A = doc.>?>(element(s)) match {
      case Some(v) => f(v)
      case None => a
    }

    def flatMap(s: String): List[Element] = doc.>?>(elementList(s)) match {
      case Some(v) => v
      case None => throw new Exception(s)
    }
  }

  implicit final class StrWrap(str: String) {
    def doc = JsoupBrowser().parseString(str)
  }

  trait Cacheable[B] {
    def contains(s: String): Boolean
    def apply(s: String): B
    def put(s: String, b: B): Unit
  }

  def get[A <: Cacheable[B], B](url: String, cache: A) (f: Browser#DocumentType => B): B =
    if (cache.contains(url)) {
      val value = cache.apply(url)
      scribe.info(s"Hit cache for key $url -> $value")
      value
    }
    else if (httpCache.contains(url)) {
      scribe.info(s"Missed item cache for $url but hit http cache")
      val html = httpCache(url)
      val result = f(JsoupBrowser().parseString(Brotli.decompress(html)))
      scribe.info(s"Got key $url -> $result")
      cache.put(url, result)
      result
    } else {
      scribe.info(s"Missed http cache... calling $url")
      val html = retryHttpGet(url)
      httpCache.put(url, Brotli.compress(html))
      val result = f(JsoupBrowser().parseString(html))
      scribe.info(s"After HTTP request, got key $url -> $result")
      cache.put(url, result)
      result
    }

  def get[A](s: String)
            (cont: String => Boolean)
            (appl: String => A)
            (pu: (String, A) => Unit)
            (f: Browser#DocumentType => A): A =
    get[Cacheable[A], A](s, new Cacheable[A] {
      override def contains(s: String): Boolean = cont(s)
      override def apply(s: String): A = appl(s)
      override def put(s: String, b: A): Unit = pu(s, b)
    }) (f)

  def main(args: Array[String]): Unit = {
    locally {
      val pages = 1 to 318
      pages.par.foreach{a =>
        val url = s"https://www.anime-planet.com/anime/all?page=$a"
        val html = retryHttpGet(url)
        val doc = html.doc

        doc.flatMap("div#siteContainer > ul.cardDeck > li.card").map{b =>
          val doc2 = b.innerHtml.doc
          val img = doc2.map("a[title] > div.crop > img[src]", url).attr("src")
          val title = doc2.map("a[title] > h4").text
          val link = doc2.map("a[href]").attr("href")
          val inner = doc2.map("a[title]").attr("title").doc

          val `type` = inner.map("ul.entryBar > li.type").text
          val year = inner.map("ul.entryBar > li.iconYear").text
          val studio = inner.map("ul.entryBar > li:nth-child(2)").text
          val rating = inner.map("ul.entryBar > li.iconYear > li > div.ttRating", url).text.toDouble
          val desc = inner.map("p").text

          val genres = inner.flatMap("div.tags > ul > li").map(_.text).toSet
          val fullTitle = AnimeTitle(
            title,
            img,
            link,
            desc,
            studio,
            year,
            rating,
            `type`,
            genres
          )

          scribe.info(s"Adding item $fullTitle")

          itemCache.animeTitles.add(fullTitle)
        }
      }
    }


    scribe.info("done")
  }
}
