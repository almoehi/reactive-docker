package com.kolor.docker.api

import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._
import com.kolor.docker.api.types._
import com.kolor.docker.api.json.Formats._

object DockerIteratee {
  
  def readLine = for {
	  lines <- Enumeratee.breakE[Parsing.MatchInfo[Array[Byte]]](_.isMatch) ><>
		    Enumeratee.collect{ 
		      case Parsing.Unmatched(bytes) => new String(bytes)
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
    
    
  val json: Iteratee[Array[Byte], List[JsObject]] = {
	Parsing.search("\n".getBytes) ><>
    Enumeratee.grouped( readLine ) ><>
	  Enumeratee.collect{
	    case str => Json.parse(s"[${str.replaceAll("""\}\{""", "},{")}]")
	  } &>>
	  Iteratee.getChunks.map{js => 
	      js.flatMap{
	      	case o:JsObject => Seq(o)
	      	case a:JsArray => a.value.filter{
	      	  case o:JsObject => true
	      	  case _ => false
	      	}.map(_.as[JsObject])
	      	case _ => Seq.empty
	      }
	  }
  }
  
  val statusStream: Iteratee[Array[Byte], Seq[Either[DockerErrorInfo, DockerStatusMessage]]] = {
	Parsing.search("\n".getBytes) ><>
    Enumeratee.grouped( readLine ) ><>
	  Enumeratee.collect{
	    case str => Json.parse(s"[${str.replaceAll("""\}\{""", "},{")}]")
	  } ><>
	  Enumeratee.map[JsValue](Json.fromJson[Seq[DockerStatusMessage]](_)) ><>
	  Enumeratee.collect[JsResult[Seq[DockerStatusMessage]]]{
	    case JsSuccess(value, _) => value
	  } ><>
	  Enumeratee.map[Seq[DockerStatusMessage]]{list => 
	    list.map{
	      case msg if (msg.error.nonEmpty) => Left(msg.error.get)
	      case msg => Right(msg)
	    }
	  } &>>
	  Iteratee.getChunks.map{list =>
	  		list.flatMap{s => s}
	  }
  }
  
  def toStatusStreamEnumerator(en: Enumerator[Array[Byte]]): Enumerator[Seq[Either[DockerErrorInfo, DockerStatusMessage]]] = {
	  en &> Parsing.search("\n".getBytes) &>
	    	Enumeratee.grouped( DockerIteratee.readLine ) &>
	    	Enumeratee.collect{
		    	case str => Json.parse(s"[${str.replaceAll("""\}\{""", "},{")}]")
	    	} &>
	    	Enumeratee.collect { 
	    	  case arr:JsArray =>  arr.value.map(Json.fromJson[DockerStatusMessage](_)) 
	    	} &>
	    	Enumeratee.collect[Seq[JsResult[DockerStatusMessage]]] { 
	    	   case list => list.filter{
	    	     case JsSuccess(value, _) => true
	    	     case _ => false
	    	   }.map(_.get)
	    	} &>
	    	Enumeratee.map[Seq[DockerStatusMessage]]{list =>
	    	  list.map {
	    	  	case msg if msg.error.nonEmpty => Left(msg.error.get)
	    	  	case msg => Right(msg)
	    	  }
	    	} 
	    	//&> Enumeratee.mapFlatten[Either[DockerErrorInfo,DockerStatusMessage]](x => Enumerator(x, x))
  }
  
  /*
  def toDockerRawStreamEnumerator(en: Enumerator[Array[Byte]]): Enumerator[DockerRawStreamChunk] = {
    val buff = java.nio.ByteBuffer.allocate(4).order(java.nio.ByteOrder.BIG_ENDIAN)

    
    def parseHeader(data: Array[Byte]): (Int,Long) = {
      val streamType = data(0).toInt
      buff.put(data.slice(4, 7))
      buff.flip
      val sz = buff.getLong
      buff.clear
      (streamType, sz)
    }
    
    val readHeader = (Enumeratee.take[Array[Byte]](8) &>>  Iteratee.consume())
    
    en &> Enumeratee.takeWhile[Array[Byte]]{
      case data => true
    }
  }
  */
}