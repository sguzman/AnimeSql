package com.github.sguzman.anime.protoc

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.SocketTimeoutException

import com.github.sguzman.brotli.Brotli
import com.google.protobuf.ByteString
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
      val input = new FileInputStream(file)
      val hash = http.HttpCache.parseFrom(input)
      input.close()

      TrieMap[String, Array[Byte]](hash.cache.toSeq.map(a => (a._1, a._2.toByteArray)): _*)
    }
  }

  def writeHttpCache(): Unit = {
    scribe.info("Writing http.msg...")
    val file = new File("./http.msg")
    val output = new FileOutputStream(file)
    http.HttpCache(httpCache.toSeq.map(a => (a._1, ByteString.copyFrom(a._2))).toMap).writeTo(output)
    output.close()

    scribe.info("Wrote http.msg")
  }
}
