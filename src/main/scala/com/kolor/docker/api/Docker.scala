package com.kolor.docker.api

import dispatch._
import Defaults._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import com.kolor.docker.api.types.ContainerId
import org.slf4j.LoggerFactory
import scala.concurrent.Promise
import play.api.libs.iteratee._
import com.ning.http.client.HttpResponseHeaders

sealed trait DockerClient extends DockerApi {  
  private val log = LoggerFactory.getLogger(this.getClass());
  
  implicit def docker:DockerClient = this
  
  def dockerApiVersion: String
  def dockerHost: String
  def dockerPort: Int
  
  final def dockerJsonRequest[T](req: dispatch.Req)(implicit docker: DockerClient, fmt: Format[T]): Future[Either[Throwable, T]] = {
    Http(req).either.map{
      case Right(resp) if (Seq(200, 201, 202).contains(resp.getStatusCode())) => Json.parse(resp.getResponseBody()).validate[T].fold(
    		  errors => Left(new DockerResponseParseError(s"Json parse errors: ${errors.mkString("|")}", docker, resp.getResponseBody())),
    		  data => Right(data) 
      )
      case Right(resp) if (resp.getStatusCode() == 409) => throw new DockerConflictException(s"docker conflict (${req.url}) : ${resp.getResponseBody()}", docker)
      //case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(ContainerId.emptyId, docker) {
      //  override def message = resp.getResponseBody()
      //}
      case Right(resp) if (resp.getStatusCode() == 500) => throw new DockerRequestException(s"docker internal server error for ${req.url} (${req.url})", docker, None, Some(req))
      case Right(resp) => throw new DockerRequestException(s"docker request error for ${req.url} (Code: ${resp.getStatusCode()}) : ${resp.getResponseBody()}", docker, None, Some(req)) 
      case Left(t) => Left(t)
    }.recover {
      case t: Throwable => 
        log.debug(s"(${req.toRequest.getMethod()}) dockerJsonRequest for ${req.url} failed", t)
        Left(t)
    }
  }
  
  final def dockerRequest(req: dispatch.Req)(implicit docker: DockerClient): Future[Either[Throwable, com.ning.http.client.Response]] = {
    Http(req).either.recover{
      case t: Throwable =>
        log.debug(s"(${req.toRequest.getMethod()}) dockerRequest for ${req.url} failed", t)
        Left(t)
    }
  }
  
  /*
  final def dockerRequestEnumerate(req: dispatch.Req)(implicit docker: DockerClient): Future[Either[Throwable, Enumerator[Array[Byte]]]] = {
    
    Http(req > as.Response(_.getResponseBodyAsStream())).either.map{
      case Right(stream) => 
        log.info(s"enumerate response of ${req.url}")
        Right(Enumerator.fromStream(stream))
      case Left(t) => Left(t)
    }.recover {
      case t: Throwable => 
        log.error(s"(${req.toRequest.getMethod()}) dockerRequestEnumerate for ${req.url} failed", t)
        Left(t)
    }
  }
  */
  
  final def dockerRequestEnumerate(req: dispatch.Req)(implicit docker: DockerClient): Future[Either[Throwable, (Int, HttpResponseHeaders, Enumerator[Array[Byte]])]] = { 
    
    val parser = ByteStream.apply
    val enumerator = parser.enumerate
    val reqFuture = Http(req > parser).recover {
      case t:Throwable => 
        log.debug(s"(${req.toRequest.getMethod()}) dockerRequestEnumerate for ${req.url} failed", t)
        throw t
    }
    
    val en = enumerator &> Enumeratee.onIterateeDone[Array[Byte]]{ () =>
		log.debug(s"Iteratee is done - try to abort connection to ${req.url}")
		if (!reqFuture.isCompleted) {
			log.debug(s"stopping WS call to ${req.url}")
			parser.stop
		} else {
			log.debug(s"Connection to ${req.url} already finished")
		}
	}
    
	val futureResult = for {
		status <- parser.promiseStatus.future
		headers <- parser.promiseHeader.future
	} yield {
	  (status, headers, en)
	}
	
	val eitherResult: Future[Either[Throwable, (Int, HttpResponseHeaders, Enumerator[Array[Byte]])]] = futureResult.map(r => Right(r)).recoverWith{
	  case t:Throwable => Future.successful(Left(t))
	}
    
    eitherResult
  }
}

sealed case class DockerClientV19(dockerHost: String, dockerPort: Int) extends DockerClient {
  final val dockerApiVersion: String = "1.9"
}


object Docker {
  def apply(host: String): DockerClient = {
    DockerClientV19(host, 4243)
  }
  
  def apply(host: String, port: Int): DockerClient = {
    DockerClientV19(host, port)
  }
  
}