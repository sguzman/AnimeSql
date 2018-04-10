package com.github.sguzman.anime.sql

import java.io.{File, FileInputStream, FileOutputStream}

import com.github.sguzman.anime.protoc.items._
import net.ruippeixotog.scalascraper.browser.{Browser, JsoupBrowser}
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.model.Element
import net.ruippeixotog.scalascraper.scraper.ContentExtractors.{element, elementList}
import org.apache.commons.lang3.StringUtils
import slick.jdbc.PostgresProfile.api._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Failure

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
      result
    } else {
      val html = HUtil.retryHttpGet(url)
      val result = f(html.doc)
      scribe.info(s"After HTTP request, got key $url -> $result")
      result
    }

  def get[A](s: String)
            (cont: String => Boolean)
            (appl: String => A)
            (f: Browser#DocumentType => A): A =
    get[Cacheable[A], A](s, new Cacheable[A] {
      override def contains(s: String): Boolean = cont(s)
      override def apply(s: String): A = appl(s)
    }) (f)

  def extract[A](s: String, cache: mutable.Map[String, A])(doc: Browser#DocumentType => A): A =
    get[A](s)(cache.contains)(cache.apply)(doc)

  implicit final class FutureWrap[A](future: Future[A]) {
    def v = Await.result(future, Duration.Inf)
  }

  def main(args: Array[String]): Unit = {
    identity {
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
      true
    } && identity {
      val animes = itemCache.sums.par.map{a =>
        val url = s"https://www.anime-planet.com${a.link}"

        val anime = get(url)(itemCache.cache.contains)(itemCache.cache.apply){doc =>
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
      true
    } && identity {
      val users = itemCache.cache.par.map{a =>
        val url = s"https://www.anime-planet.com/ajaxDelegator.php?mode=stats&type=anime&id=${a._2.id}&url=${a._1.afterLast("/")}"

        val user = get(url)(itemCache.anime.contains)(itemCache.anime.apply){doc =>
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
      true
    } && identity {
      final class Genres(tag: Tag) extends Table[(Int, String)](tag, "genres") {
        def id = column[Int]("genre_id", O.Unique, O.PrimaryKey, O.AutoInc)
        def name = column[String]("name")

        def * = (id, name)
      }
      val genres = TableQuery[Genres]

      final class Summary(tag: Tag) extends Table[(Int, String, String, String, String, String, Double, String)](tag, "summary") {
        def id = column[Int]("summary_id", O.Unique, O.PrimaryKey, O.AutoInc)
        def title = column[String]("title")
        def img = column[String]("img")
        def link = column[String]("link", O.Unique, O.Length(200))
        def desc = column[String]("desc")
        def studio = column[String]("studio")
        def rating = column[Double]("rating")
        def showType = column[String]("showType")

        def * = (id, title, img, link, desc, studio, rating, showType)
      }
      val summary = TableQuery[Summary]

      final class List(tag: Tag) extends Table[(Int, Int, Int)](tag, "list") {
        def id = column[Int]("id", O.Unique, O.PrimaryKey, O.AutoInc)
        def sumId = column[Int]("sum_id")
        def genId = column[Int]("gen_id")

        def * = (id, sumId, genId)
      }
      val list = TableQuery[List]

      val db = Database.forURL("jdbc:postgresql://localhost:5432/postgres", driver = "org.postgresql.Driver", user = "alice", password = "pass")

      identity {
        val tables = List(genres, summary, list)

        tables.par.foreach{a =>
          util.Try(db.run(DBIO.seq(a.schema.create)).v) match {
            case Failure(e) => println(s"Could not create db ${e.getMessage}")
            case _ => println("Successfully created db")
          }
        }

        true
      } && identity {
        val genresList = db.run(genres.result.map(_.map(a => a._2))).v.toSet
        val genreList = itemCache.cache.values.flatMap(_.getSummary.genres).toSet

        val diff = genreList.diff(genresList)
        db.run(DBIO.seq(
          genres ++= diff.map((0, _))
        )).v
        true
      } && identity {
        val summaries = db.run(summary.result.map(_.map(a => (0, a._2, a._3, a._4, a._5, a._6, a._7, a._8)))).v.toSet
        val summaries2 = itemCache.cache.values.map(_.getSummary).map(a => (0, a.title, a.img, a.link, a.desc, a.studio, a.rating, a.showType)).toSet
        val diff = summaries2.diff(summaries)

        db.run(DBIO.seq(
          summary ++= diff
        )).v
        true
      } && identity {
        val genresList = db.run(genres.result.map(_.map(a => a._2 -> a._1))).v.toMap
        val summaries = db.run(summary.result.map(_.map(a => a._4 -> a._1))).v.toMap

        val sumGen = db.run(list.result.map(_.map(a => (a._2, a._3)))).v.toSet
        val sumGen2 = itemCache.cache.values.map(_.getSummary).flatMap(a => a.genres.map(b => (summaries(a.link), genresList(b)))).toSet
        val diff = sumGen2.diff(sumGen)
        db.run(DBIO.seq(
          list ++= diff.map(a => (0, a._1, a._2))
        )).v
        true
      }

      db.close()
      true
    }

    scribe.info("done")
  }
}
