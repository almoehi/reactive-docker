package com.kolor.docker.api

import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import com.kolor.docker.api.types._
import com.kolor.docker.api.json.Formats._
import org.slf4j.LoggerFactory
import play.api.libs.iteratee.Iteratee
import play.api.libs.iteratee.Input.{El, Empty, EOF}

object DockerEnumeratee {
  
  protected val log = LoggerFactory.getLogger(this.getClass());
  
  def statusStream(charset: String = "utf-8"): Enumeratee[Array[Byte], Either[DockerErrorInfo, DockerStatusMessage]] = {    
    		Enumeratee.map[Array[Byte]]{chr => 
    		  	val str = (new String(chr, charset)).trim
    			log.debug(s"statusStreamEnumeratee decoded: '${str}'")
    		  	str
    		} ><>
    		Enumeratee.collect[String]{
    		  case str if (str.nonEmpty) => str.toCharArray
    		} ><>
//    		play.extras.iteratees.Combinators.errorReporter ><>
			Enumeratee.grouped(play.custom.iteratees.JsonIteratees.jsSimpleObject) ><>
			Enumeratee.collect[JsObject]{
			  case obj if (obj.fields.size > 0) => obj
			} ><>
			Enumeratee.map[JsObject]{json =>
			  	log.debug(s"statusStreamEnumeratee simpleObject: ${json}")
				com.kolor.docker.api.json.Formats.dockerStatusMessageFmt.reads(json)
			} ><>
			Enumeratee.collect[JsResult[DockerStatusMessage]] { 
			  case JsSuccess(value, _) => value
			  case JsError(err) => 
			    log.error(s"statusStreamEnumeratee JSON parse error: " + err.mkString)
			    DockerStatusMessage(error = Some(DockerErrorInfo(Some(-1), Some(s"unable to parse JSON: " + err.mkString))))
			} ><>
			Enumeratee.map[DockerStatusMessage]{status => status match {
	    	  	case msg if (msg.error.nonEmpty && !msg.isError) => 
	    	  	  val msg = status.copy(error = None)
	    	  	  log.debug("statusStreamEnumeratee empty errorMsg mapped to statusMsg: " + msg)
	    	  	  Right(msg)
	    	  	case msg if msg.isError => 
	    	  	  log.debug("statusStreamEnumeratee mapped errorMsg: " + msg.error.get)
	    	  	  Left(msg.error.get)
	    	  	case msg => 
	    	  	  log.debug("statusStreamEnumeratee mapped statusMsg: " + msg)
	    	  	  Right(msg)
			  }
	    	}
  }
  
  def rawStream: Enumeratee[Array[Byte], DockerRawStreamChunk] = {
    /*
    val buff = java.nio.ByteBuffer.allocate(4).order(java.nio.ByteOrder.BIG_ENDIAN)
    def parseHeader(data: Array[Byte]): (Int,Long) = {
      val streamType = data(0).toInt
      buff.put(data.slice(4, 7))
      buff.flip
      val sz = buff.getLong
      buff.clear
      (streamType, sz)
    }
    */

    def read(n: Int): Iteratee[Array[Byte], Array[Byte]] = Cont {
      case in @ EOF => Done(Array.empty, in)
      case Empty => read(n)
      case El(data) => {
        val remaining = n - data.length
        if (remaining == 0) {
          Done(data, Empty)
        } else if (remaining < 0) {
          Done(data.take(n), El(data.drop(n)))
        } else {
          read(remaining)
        }
      }
    }

    def getInt(arr: Array[Byte]): Int = {
	    arr(0)<<24 | arr(1)<<16 | arr(2)<<8 | arr(3)
    }

    def header = read(8).map{
    	case hdr if (hdr.length == 8) => (hdr.drop(1).head.toInt, getInt(hdr.drop(4)))
      case _ => (-1, 0) // set stream to -1 indicates error / empty response
    }

    def chunk = for {
	    header <- header
	    data <- read(header._2)
	  } yield {
	    //log.info(s"RawChunk parsed: channel=${header._1.toInt} size=${header._2.toInt} data='${data.map("%02x".format(_)).mkString(" ")}'")
	    DockerRawStreamChunk(header._1.toInt, header._2, data)
	  }

    /*
    def rawStream = for {
	    chunks <- Iteratee.repeat(chunk)
    } yield {
	    chunks
    }
    */
//    
//    val rawStreamParser = Iteratee.flatten(en |>> rawStream)
    
      Enumeratee.grouped(chunk) ><>
      Enumeratee.collect[DockerRawStreamChunk]{
        case chunk if (chunk.channel >= 0) => chunk   // filter empty/error chunks
      }
  }
  
}