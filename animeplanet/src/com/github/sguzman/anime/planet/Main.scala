package com.github.sguzman.anime.planet

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.SocketTimeoutException

import org.msgpack.core.{MessagePack, MessagePacker, MessageUnpacker}
import scalaj.http.Http

import scala.util.{Failure, Success}

object Main {
  private implicit final class PackerWrap(msg: MessagePacker) {
    def binary(binary: Array[Byte]): Unit = {
      msg.packBinaryHeader(binary.length)
      msg.addPayload(binary)
    }

    def httpMap(): Unit = {
      msg.packMapHeader(httpCache.size)
      httpCache.keysIterator.foreach{key =>
        msg.packString(key)
        msg.binary(httpCache(key))
      }

      msg.close()
    }
  }

  private implicit final class UnpackerWrap(msg: MessageUnpacker) {
    def binary: Array[Byte] = {
      val valueLen = msg.unpackBinaryHeader
      msg.readPayload(valueLen)
    }

    def httpMap(idx: Int = 0): Map[String, Array[Byte]] = {
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

  val httpCache: Map[String, Array[Byte]] = identity {
    val file = new File("./http.msg")
    if (!file.exists) {
      file.createNewFile()
      Map()
    } else {
      MessagePack.newDefaultUnpacker(new FileInputStream(file)).httpMap()
    }
  }

  def writeHttpCache(): Unit = {
    val file = new File("./http.msg")
    MessagePack.newDefaultPacker(new FileOutputStream(file)).httpMap()
  }

  Runtime.getRuntime.addShutdownHook(new Thread(() => {
    writeHttpCache()
  }))

  def retryHttpGet(url: String): String = util.Try(Http(url).asString) match {
    case Success(v) => v.body
    case Failure(e) => e match {
      case _: SocketTimeoutException => retryHttpGet(url)
      case _ => throw new Exception(s"Url: $url; ${e.getMessage}")
    }
  }

  def main(args: Array[String]): Unit = {
    val pages = 1 to 318

    scribe.info("done")
  }
}
