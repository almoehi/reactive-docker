package com.kolor.docker.api

import dispatch._
import Defaults._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import com.kolor.docker.api.types.ContainerId
import org.slf4j.LoggerFactory

sealed trait DockerClient extends DockerApi {  
  protected val log = LoggerFactory.getLogger(this.getClass());
  
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
        println(t.getStackTraceString)
        Left(t)
    }
  }
  
  final def dockerRequest(req: dispatch.Req)(implicit docker: DockerClient): Future[Either[Throwable, com.ning.http.client.Response]] = {
    Http(req).either
  }
  
  final def dockerRequestStream(req: dispatch.Req)(implicit docker: DockerClient): Future[Either[Throwable, Enumerator[Array[Byte]]]] = {
    Http(req.>(_.getResponseBodyAsStream())).either.map{
      case Right(stream) => Right(Enumerator.fromStream(stream))
      case Left(t) => Left(t)
    }
  }
}

sealed case class DockerClientV19(dockerHost: String, dockerPort: Int) extends DockerClient {
  final val dockerApiVersion: String = "1.9"
  implicit val docker = this
}


object Docker {
  def apply(host: String): DockerClient = {
    DockerClientV19(host, 4243)
  }
  
  def apply(host: String, port: Int): DockerClient = {
    DockerClientV19(host, port)
  }
  
}