package com.github.sguzman.anime.protoc

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.SocketTimeoutException

import com.github.sguzman.brotli.Brotli
import scalaj.http.Http

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success}

object HUtil {
  def httpCacheQuery(url: String): String =
    if (httpCache.contains(url)) {
      scribe.info(s"Hit http cache with key $url")
      Brotli.decompress(httpCache(url))
    } else {
      scribe.info(s"Miss http cache with key $url")
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
}
