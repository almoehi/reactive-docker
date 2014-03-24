package com.kolor.docker.api

import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import com.kolor.docker.api.types._
import com.kolor.docker.api.json.Formats._
import org.slf4j.LoggerFactory
import play.api.libs.iteratee.Iteratee

object DockerIteratee {
  
  protected val log = LoggerFactory.getLogger(this.getClass());
  
  def readLine = for {
	  lines <- Enumeratee.breakE[Parsing.MatchInfo[Array[Byte]]](_.isMatch) ><>
		    Enumeratee.collect{ 
		      case Parsing.Unmatched(bytes) => 
		        val s = new String(bytes)
		        //log.info(s"readLine $s")
		        s
		    } &>>
		    Iteratee.getChunks
	  _ <- Enumeratee.take(1) &>> Iteratee.ignore[Parsing.MatchInfo[Array[Byte]]]
  } yield lines.filter(_.nonEmpty).mkString

  val concatLine: Iteratee[Parsing.MatchInfo[Array[Byte]],String] = 
  (
    Enumeratee.breakE[Parsing.MatchInfo[Array[Byte]]](_.isMatch) ><>
    Enumeratee.collect{ 
      case Parsing.Unmatched(bytes) => new String(bytes)
    } &>>
    Iteratee.consume()
  )
  .flatMap(r => Iteratee.head.map(_ => r))
    
  
  val statusStreamIteratee = {
    //play.extras.iteratees.Encoding.decode() ><>
			//Enumeratee.grouped(play.extras.iteratees.JsonParser.jsonObject) ><>
			//play.extras.iteratees.JsonEnumeratees.jsValue ><>
			//play.extras.iteratees.JsonEnumeratees.jsObject(play.extras.iteratees.JsonIteratees.jsValues(play.extras.iteratees.JsonIteratees.jsValue)) ><> 
			Enumeratee.map[Array[Byte]](c => Json.parse(c.map(_.toByte))) ><>
			Enumeratee.map[JsValue](com.kolor.docker.api.json.Formats.dockerStatusMessageFmt.reads(_)) ><>
			Enumeratee.collect[JsResult[DockerStatusMessage]] { 
			  case JsSuccess(value, _) => value
			  case JsError(err) => 
			    log.error(s"statusStreamIteratee JSON parse error: " + err.mkString)
			    DockerStatusMessage(error = Some(DockerErrorInfo(Some(-1), Some(s"unable to parse JSON: " + err.mkString))))
			} ><>
			Enumeratee.map[DockerStatusMessage]{status => status match {
	    	  	case msg if (msg.error.nonEmpty && !msg.isError) => 
	    	  	  val msg = status.copy(error = None)
	    	  	  log.debug("statusStreamIteratee empty errorMsg mapped to statusMsg: " + msg)
	    	  	  Right(msg)
	    	  	case msg if msg.isError => 
	    	  	  log.debug("statusStreamIteratee mapped errorMsg: " + msg.error.get)
	    	  	  Left(msg.error.get)
	    	  	case msg => 
	    	  	  log.debug("statusStreamIteratee mapped statusMsg: " + msg)
	    	  	  Right(msg)
			  }
	    	} &> Iteratee.getChunks
  }
  
  def asResponseBody(en: Enumerator[Array[Byte]]): Enumerator[String] = {
    (en &> Parsing.search("\n".getBytes) &> Enumeratee.grouped(readLine) &> Enumeratee.collect{
      case s => s
    })
  }
  
  def enumerateStatusStream(en: Enumerator[Array[Byte]]): Enumerator[Either[DockerErrorInfo, DockerStatusMessage]] = {
	en &> //play.extras.iteratees.Encoding.decode() &>
			//Enumeratee.map[Array[Byte]](_.map(_.toChar)) &>
			//Enumeratee.grouped(play.extras.iteratees.JsonIteratees.jsValue) &> 
			//play.extras.iteratees.JsonEnumeratees.jsObject(play.extras.iteratees.JsonIteratees.jsValues(play.extras.iteratees.JsonIteratees.jsValue)) &> 
			//play.extras.iteratees.JsonEnumeratees.jsObject &>
			//Enumeratee.map[(String, play.api.libs.json.JsValue)](Json.obj(_)) &>
			Enumeratee.map[Array[Byte]]{c => 
				try {
				  Json.parse(c)
				} catch {
				  case t:Throwable => 
				    log.error(s"Json.parse failed: $c", t)
				    Json.obj("message" -> t.getLocalizedMessage())
				}
			} &>
			Enumeratee.map[JsValue](com.kolor.docker.api.json.Formats.dockerStatusMessageFmt.reads(_)) &>
			Enumeratee.collect[JsResult[DockerStatusMessage]] { 
			  case JsSuccess(value, _) => value
			  case JsError(err) => 
			    log.error(s"enumerateStatusStream JSON parse error: " + err.mkString)
			    DockerStatusMessage(error = Some(DockerErrorInfo(Some(-1), Some(s"unable to parse JSON: " + err.mkString))))
			} &>
			Enumeratee.map[DockerStatusMessage]{status => status match {
	    	  	case msg if (msg.error.nonEmpty && !msg.isError) => 
	    	  	  val msg = status.copy(error = None)
	    	  	  log.debug("enumerateStatusStream empty errorMsg mapped to statusMsg: " + msg)
	    	  	  Right(msg)
	    	  	case msg if msg.isError => 
	    	  	  log.debug("enumerateStatusStream mapped errorMsg: " + msg.error.get)
	    	  	  Left(msg.error.get)
	    	  	case msg => 
	    	  	  log.debug("enumerateStatusStream mapped statusMsg: " + msg)
	    	  	  Right(msg)
			  }
	    	}
  }
  
  
  def rawStreamEnumerator(en: Enumerator[Array[Byte]]): Enumerator[DockerRawStreamChunk] = {
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
    
    def getInt(arr: Array[Byte]): Int = {
	    arr(0)<<24 | arr(1)<<16 | arr(2)<<8 | arr(3)
    }
    
    def read(n: Int) = (Enumeratee.take[Array[Byte]](n) &>>  Iteratee.consume())
    
    def header = read(8).map{hdr => 
    	(hdr(0).toInt, getInt(hdr.slice(4,7)))
    }
    
    def chunk = for {
	    header <- header
	    data <- read(header._2)
	  } yield {
	    DockerRawStreamChunk(header._1.toInt, header._2, data)
	  }
  
    def rawStream = for {
	    chunks <- Iteratee.repeat(chunk)
    } yield {
	    chunks
    }
    
    val rawStreamParser = Iteratee.flatten(en |>> rawStream)
    
    en &> 
      Enumeratee.grouped(rawStreamParser) &>
      Enumeratee.collect{
      	case chunks => chunks
      } &>
      Enumeratee.mapFlatten[Seq[DockerRawStreamChunk]]( d => Enumerator.apply(d : _*) )
  }
  
}