package com.github.sguzman.anime.sql

import java.io.{File, FileInputStream, FileOutputStream}

import com.github.sguzman.anime.protoc.items._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.elementList
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.element
import org.apache.commons.lang3.StringUtils

import scala.collection.mutable

object Main {
  var itemCache: Items = identity {
    val file = new File("./items.msg")
    if (!file.exists) {
      scribe.info("Creating items.msg file")
      file.createNewFile()
      Items(Seq(), Map(), Map())
    } else {
      scribe.info("Found items.msg file")
      val input = new FileInputStream(file)
      val out = Items.parseFrom(input)
      input.close()
      out
    }
  }

  def writeItemCache(): Unit = {
    scribe.info("Writing items.msg...")
    val file = new File("./items.msg")
    val output = new FileOutputStream(file)
    itemCache.writeTo(output)
    output.close()

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
    def afterLast(sep: String) = StringUtils.substringAfterLast(str, sep)
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
      val shows = pages.par.flatMap{a =>
        val url = s"https://www.anime-planet.com/anime/all?page=$a"
        val html = HUtil.retryHttpGet(url)
        val doc = html.doc

        doc.flatMap("div#siteContainer > ul.cardDeck > li.card").map{ b =>
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

          val genres = inner.flatMap("div.tags > ul > li").map(_.text)
          val fullTitle = AnimeSummary(title, img, link, desc, studio, year, rating, `type`, genres)
          fullTitle
        }
      }.toList
      itemCache = itemCache.addAllSums(shows)
    }

    locally {
      val animes = itemCache.sums.par.map{a =>
        val url = s"https://www.anime-planet.com${a.link}"

        val anime = get(url)(itemCache.cache.contains)(itemCache.cache.apply)((_, _) => {}) {doc =>
          val alt = doc.maybe("h2.aka").map(_.text).getOrElse("")
          println(a.link)
          val rawRank = doc.map("#siteContainer > section.pure-g.entryBar > div:nth-child(5)").text
          val rank = if(rawRank.isEmpty) -1
          else rawRank.after("#")
            .replaceAll(",", "")
            .trim
            .toInt

          val id = doc.map("""form[data-mode="anime"]""").attr("data-id").toLong
          val u = a.link.stripPrefix("/anime/")

          Anime(Some(a), alt, rank, id, u)
        }

        url -> anime
      }.toList

      itemCache = itemCache.addAllCache(animes)
    }

    locally {
      val users = itemCache.cache.par.map{a =>
        val url = s"https://www.anime-planet.com/ajaxDelegator.php?mode=stats&type=anime&id=${a._2.id}&url=${a._1.afterLast("/")}"

        val user = get(url)(itemCache.anime.contains)(itemCache.anime.apply)((_, _) => {}) {doc =>
          val watched = doc.maybe("ul.statList > li.status1 > a > span.slCount").map(_.text.replaceAll(",","").toInt).getOrElse(0)
          val watching = doc.map("ul.statList > li.status2 > a > span.slCount").text.replaceAll(",","").toInt
          val wantToWatch = doc.map("ul.statList > li.status3 > a > span.slCount").text.replaceAll(",","").toInt
          val stalled = doc.map("ul.statList > li.status4 > a > span.slCount").text.replaceAll(",","").toInt
          val dropped = doc.map("ul.statList > li.status5 > a > span.slCount").text.replaceAll(",","").toInt
          val wontWatch = doc.map("ul.statList > li.status6 > a > span.slCount").text.replaceAll(",","").toInt

          AnimeUser(Some(a._2), Some(UserStats(watched, watching, wantToWatch, stalled, dropped, wontWatch)))
        }

        url -> user
      }.toList

      itemCache = itemCache.addAllAnime(users)
    }

    scribe.info("done")
  }
}
