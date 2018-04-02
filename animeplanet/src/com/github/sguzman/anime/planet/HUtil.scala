package com.github.sguzman.anime.planet

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.SocketTimeoutException

import com.github.sguzman.brotli.Brotli
import org.msgpack.core.{MessagePack, MessagePacker, MessageUnpacker}
import scalaj.http.Http

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success}

object HUtil {
  private implicit final class PackerWrap(msg: MessagePacker) {
    def binary(binary: Array[Byte]): Unit = {
      msg.packBinaryHeader(binary.length)
      val _ = msg.addPayload(binary)
    }

    def httpMap(): Unit = {
      msg.packMapHeader(HUtil.httpCache.size)
      HUtil.httpCache.keysIterator.foreach{ key =>
        msg.packString(key)
        msg.binary(HUtil.httpCache(key))
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
  }

  def httpCacheQuery(url: String): String =
    if (httpCache.contains(url)) {
      scribe.info(s"Hit http cache with key $url")
      Brotli.decompress(httpCache(url))
    } else {
      HUtil
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
