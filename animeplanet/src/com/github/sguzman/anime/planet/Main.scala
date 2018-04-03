package com.github.sguzman.anime.planet

import java.io.{File, FileInputStream, FileOutputStream}

import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elementList}
import org.apache.commons.lang3.StringUtils
import org.msgpack.core.{MessagePack, MessagePacker, MessageUnpacker}

import scala.collection.mutable

object Main {
  private implicit final class PackerWrap(msg: MessagePacker) {
    def binary(binary: Array[Byte]): Unit = {
      msg.packBinaryHeader(binary.length)
      val _ = msg.addPayload(binary)
    }

    def user(user: UserStats): Unit = {
      msg.packInt(user.watched)
      msg.packInt(user.watching)
      msg.packInt(user.wantToWatch)
      msg.packInt(user.stalled)
      msg.packInt(user.dropped)
      val _ = msg.packInt(user.wontWatch)
    }

    def summary(a: AnimeSummary): Unit = {
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

    def items(): Unit = {
      val list = itemCache.animeTitles.toList
      msg.packArrayHeader(list.length)
      list.foreach(summary)

      msg.packMapHeader(itemCache.animeCache.size)
      itemCache.animeCache.foreach{a =>
        msg.packString(a._1)
        summary(a._2.summary)
        msg.packString(a._2.altTitle)
        msg.packInt(a._2.rank)
        msg.packInt(a._2.id)
        msg.packString(a._2.url)
      }

      itemCache.animeUsers.foreach{a =>
        msg.packString(a._1)
        summary(a._2.anime.summary)
        msg.packString(a._2.anime.altTitle)
        msg.packInt(a._2.anime.rank)
        msg.packInt(a._2.anime.id)
        msg.packString(a._2.anime.url)
        user(a._2.user)
      }

      msg.close()
    }
  }

  private implicit final class UnpackerWrap(msg: MessageUnpacker) {
    def binary: Array[Byte] = {
      val valueLen = msg.unpackBinaryHeader
      msg.readPayload(valueLen)
    }

    def summary: AnimeSummary = {
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

      AnimeSummary(title, img, link, desc, studio, year, rating, `type`, genres)
    }

    def user: UserStats = {
      UserStats(
        msg.unpackInt,
        msg.unpackInt,
        msg.unpackInt,
        msg.unpackInt,
        msg.unpackInt,
        msg.unpackInt
      )
    }

    def items: Items = {
      val len = msg.unpackArrayHeader
      val titles = (1 to len).map(_ => summary)

      val mapLen = msg.unpackMapHeader
      val seq = (1 to mapLen).map{_ =>
        val key = msg.unpackString
        val sum = summary
        val alt = msg.unpackString
        val rank = msg.unpackInt
        val id = msg.unpackInt
        val url = msg.unpackString

        val value = Anime(sum, alt, rank, id, url)

        key -> value
      }

      val mapLen2 = msg.unpackMapHeader
      val seq2 = (1 to mapLen2).map{_ =>
        val key = msg.unpackString
        val sum = summary
        val alt = msg.unpackString
        val rank = msg.unpackInt
        val id = msg.unpackInt
        val url = msg.unpackString

        val valueAnime = Anime(sum, alt, rank, id, url)

        key -> AnimeUsers(valueAnime, UserStats(
          msg.unpackInt,
          msg.unpackInt,
          msg.unpackInt,
          msg.unpackInt,
          msg.unpackInt,
          msg.unpackInt
        ))
      }

      val map = mutable.HashMap[String, Anime](seq: _*)
      val map2 = mutable.HashMap[String, AnimeUsers](seq2: _*)

      val is = Items(mutable.Set(titles: _*), map, map2)
      msg.close()
      is
    }
  }

  final case class AnimeSummary(
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

  final case class UserStats(
                            watched: Int,
                            watching: Int,
                            wantToWatch: Int,
                            stalled: Int,
                            dropped: Int,
                            wontWatch: Int
                            )

  final case class Anime(
                          summary: AnimeSummary,
                          altTitle: String,
                          rank: Int,
                          id: Int,
                          url: String
                        )

    final case class AnimeUsers(
                          anime: Anime,
                          user: UserStats
                        )

  final case class Items(
                          animeTitles: mutable.Set[AnimeSummary],
                          animeCache: mutable.HashMap[String, Anime],
                          animeUsers: mutable.HashMap[String, AnimeUsers]
                        )

  val itemCache: Items = identity {
    val file = new File("./items.msg")
    if (!file.exists) {
      scribe.info("Creating items.msg file")
      file.createNewFile()
      Items(mutable.Set(), mutable.HashMap(), mutable.HashMap())
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
    HUtil.writeHttpCache()
  }))

  implicit final class DocWrap(doc: Browser#DocumentType) {
    def map(s: String, a: String = ""): Element = doc.>?>(element(s)) match {
      case Some(v) => v
      case None => throw new Exception(s"$s $a")
    }

    def maybe(s: String) = doc.>?>(element(s))

    def flatMap(s: String): List[Element] = doc.>?>(elementList(s)) match {
      case Some(v) => v
      case None => throw new Exception(s)
    }
  }

  implicit final class StrWrap(str: String) {
    def doc = JsoupBrowser().parseString(str)

    def after(sep: String) = StringUtils.substringAfter(str, sep)
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
    else if (HUtil.httpCache.contains(url)) {
      val html = HUtil.retryHttpGet(url)
      val result = f(html.doc)
      scribe.info(s"Got key $url -> $result")
      cache.put(url, result)
      result
    } else {
      val html = HUtil.retryHttpGet(url)
      val result = f(html.doc)
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

  def extract[A](s: String, cache: mutable.Map[String, A])(doc: Browser#DocumentType => A): A =
    get[A](s)(cache.contains)(cache.apply)((a,b) => { val _ = cache.put(a,b) } )(doc)

  def main(args: Array[String]): Unit = {
    locally {
      val pages = 1 to 318
      pages.par.foreach{a =>
        val url = s"https://www.anime-planet.com/anime/all?page=$a"
        val html = HUtil.retryHttpGet(url)
        val doc = html.doc

        doc.flatMap("div#siteContainer > ul.cardDeck > li.card").map{b =>
          val doc2 = b.innerHtml.doc
          val img = doc2.map("a[title] > div.crop > img[src]", url).attr("src")
          val title = doc2.map("a[title] > h4").text
          val link = doc2.map("a[href]").attr("href")
          val inner = doc2.map("a[title]").attr("title").doc

          val `type` = inner.map("ul.entryBar > li.type").text
          val year = inner.maybe("ul.entryBar > li.iconYear").map(_.text).getOrElse("")
          val studio = inner.map("ul.entryBar > li:nth-child(2)").text
          val rating = inner.maybe("ul.entryBar > li > div.ttRating").map(_.text.toDouble).getOrElse(-1.0d)
          val desc = inner.map("p").text

          val genres = inner.flatMap("div.tags > ul > li").map(_.text).toSet
          val fullTitle = AnimeSummary(
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

          scribe.debug(s"Adding item $fullTitle")
          itemCache.animeTitles.add(fullTitle)
        }
      }
    }

    locally {
      itemCache.animeTitles.par.foreach{a =>
        val url = s"https://www.anime-planet.com${a.link}"
        val cache = itemCache.animeCache

        extract(url, cache) {doc =>
          val alt = doc.maybe("h2.aka").map(_.text).getOrElse("")
          println(a.link)
          val rawRank = doc.map("#siteContainer > section.pure-g.entryBar > div:nth-child(5)").text
          val rank = if(rawRank.isEmpty) -1
          else rawRank.after("#")
            .replaceAll(",", "")
            .trim
            .toInt

          val id = doc.map("""form[data-mode="anime"]""").attr("data-id").toInt
          val u = a.link.stripPrefix("/anime/")

          Anime(a, alt, rank, id, u)
        }
      }
    }

    locally (
      itemCache.animeCache.par.foreach{a =>
        val url = s"https://www.anime-planet.com/ajaxDelegator.php?mode=stats&type=anime&id=${a._2.id}&url=${a._1.stripPrefix("/").after("/")}"
        val cache = itemCache.animeUsers

        extract(url, cache) {doc =>
          val watched = doc.map("ul.statList > li.status1 > span.slCount").text.replaceAll(",","").toInt
          val watching = doc.map("ul.statList > li.status2 > span.slCount").text.replaceAll(",","").toInt
          val wantToWatch = doc.map("ul.statList > li.status3 > span.slCount").text.replaceAll(",","").toInt
          val stalled = doc.map("ul.statList > li.status4 > span.slCount").text.replaceAll(",","").toInt
          val dropped = doc.map("ul.statList > li.status5 > span.slCount").text.replaceAll(",","").toInt
          val wontWatch = doc.map("ul.statList > li.status6 > span.slCount").text.replaceAll(",","").toInt

          AnimeUsers(a._2, UserStats(watched, watching, wantToWatch, stalled, dropped, wontWatch))
        }
      }
    )

    scribe.info("done")
  }
}
