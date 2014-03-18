package com.kolor.docker.api

import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext.Implicits.global
import play.api.libs.json._

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
}