package com.kolor.docker.api

import dispatch._
import Defaults._
import play.api.libs.iteratee.Enumerator
import play.api.libs.json._
import com.kolor.docker.api.entities.ContainerId
import org.slf4j.LoggerFactory
import scala.concurrent.Promise
import play.api.libs.iteratee._
import com.ning.http.client.HttpResponseHeaders
import scala.concurrent.ExecutionContext
import com.ning.http.client.AsyncHandler
import com.ning.http.client.HttpResponseStatus
import com.ning.http.client.FluentCaseInsensitiveStringsMap
import scala.collection.immutable.TreeMap
import com.ning.http.client.HttpResponseBodyPart
import play.api.libs.iteratee.Input._


sealed case class DockerResponseHeaders(responseStatus: Int, responseText: String, headers: Map[String,Seq[String]])


sealed trait DockerClient extends DockerApi {  
  private val log = LoggerFactory.getLogger(this.getClass());
  
  implicit def docker:DockerClient = this
    
  def dockerApiVersion: String
  def dockerHost: String
  def dockerPort: Int
  
  
  private def nullConsumer(hdr: DockerResponseHeaders) =  Iteratee.ignore[Array[Byte]]
  
  final def httpRequest(req: dispatch.Req)(implicit docker: DockerClient): Future[Either[Throwable, String]] = {
    log.debug(s"httpRequest: ${req}")
    Http(req).either.map{
      case Right(resp) if (Seq(200, 201, 202).contains(resp.getStatusCode())) => Right(resp.getResponseBody())
      case Right(resp) if (resp.getStatusCode() == 409) => throw new DockerConflictException(s"docker conflict (${req.url}) : ${resp.getResponseBody()}", docker)
      case Right(resp) if (resp.getStatusCode() == 500) => throw new DockerInternalServerErrorException(docker, s"docker internal server error for ${req.url}: ${resp.getResponseBody()}")
      case Right(resp) => throw new DockerRequestException(s"docker request error for ${req.url} (Code: ${resp.getStatusCode()}) : ${resp.getResponseBody()}", docker, None, Some(req)) 
      case Left(t) => Left(t)
    }.recover {
      case t: Throwable => 
        log.debug(s"(${req.toRequest.getMethod()}) httpRequest for ${req.url} failed", t)
        Left(t)
    }
  }
  
  final def dockerJsonRequest[T](req: dispatch.Req)(implicit docker: DockerClient, fmt: Format[T]): Future[Either[Throwable, T]] = {
    log.debug(s"dockerJsonRequest: ${req}")
    Http(req).either.map{
      case Right(resp) if (Seq(200, 201, 202).contains(resp.getStatusCode())) => 
        Json.parse(resp.getResponseBody()).validate[T].fold(
    		  errors => Left(new DockerResponseParseError(s"Json parse errors: ${errors.mkString("|")}", docker, resp.getResponseBody())),
    		  data => Right(data) 
        )
      //case Right(resp) if (resp.getStatusCode() == 409) => 
      //  throw new DockerConflictException(s"docker conflict (${req.url}) : ${resp.getResponseBody()}", docker)
      //case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(ContainerId.emptyId, docker) {
      //  override def message = resp.getResponseBody()
      //}
      //case Right(resp) if (resp.getStatusCode() == 500) => throw new DockerInternalServerErrorException(docker, s"docker internal server error for ${req.url}: ${resp.getResponseBody()}")
      //case Right(resp) => throw new DockerRequestException(s"docker request error for ${req.url} (Code: ${resp.getStatusCode()}) : ${resp.getResponseBody()}", docker, None, Some(req)) 
      case Right(resp) => Left(dispatch.StatusCode(resp.getStatusCode()))
      case Left(t) => Left(t)
    }.recover {
      case t: Throwable => 
        log.debug(s"(${req.toRequest.getMethod()}) dockerJsonRequest for ${req.url} failed", t)
        Left(t)
    }
  }
  
  final def dockerRequest(req: dispatch.Req)(implicit docker: DockerClient): Future[Either[Throwable, com.ning.http.client.Response]] = {
    log.debug(s"dockerRequest: ${req}")
    Http(req).either.recover{
      case t: Throwable =>
        log.debug(s"(${req.toRequest.getMethod()}) dockerRequest for ${req.url} failed", t)
        Left(t)
    }
  }
  
  def dockerRequestIteratee[A](req: dispatch.Req)(consumer: DockerResponseHeaders => Iteratee[Array[Byte], A] = nullConsumer(_))(implicit ec: ExecutionContext): Future[Iteratee[Array[Byte], A]] = {
      log.debug(s"dockerRequestIteratee: ${req}")
	  val iterateeP = Promise[Iteratee[Array[Byte], A]]()
      var iteratee: Iteratee[Array[Byte], A] = null
      var statusCode = 0
      var statusText = ""
      var doneOrError = false
      val cancelP = Promise[Boolean]
      
      val handler = new AsyncHandler[Unit]() {
    	  import com.ning.http.client.AsyncHandler.STATE
    	  import scala.collection.JavaConverters._

    	  private def headersToMap(headers: FluentCaseInsensitiveStringsMap): Map[String,Seq[String]] = {
		    val res = scala.collection.JavaConverters.mapAsScalaMapConverter(headers).asScala.map(e => (e._1 -> e._2.asScala.toSeq)).toMap
		    TreeMap(res.toSeq: _*)(Ordering.String) //(CaseInsensitiveOrdered)
		  }
    	      	  
	      override def onStatusReceived(status: HttpResponseStatus) = {
	        statusCode = status.getStatusCode()
	        statusText = status.getStatusText()
	        
	        log.debug(s"${req.url} connected - StatusCode: $statusCode")
	        
	        if (statusCode > 300) {
	          iterateeP.failure(new DockerResponseCode(statusCode, statusText))
	          doneOrError = true
	          STATE.ABORT
	        } else {
	          STATE.CONTINUE
	        }
	        
	      }

	      override def onHeadersReceived(h: HttpResponseHeaders) = {
	        val headers = h.getHeaders()
	        iteratee = consumer(DockerResponseHeaders(statusCode, statusText, headersToMap(headers)))
	        STATE.CONTINUE
	      }

    	  override def onBodyPartReceived(bodyPart: HttpResponseBodyPart) = {
    	    
	        if (!doneOrError) {
	          iteratee = iteratee.pureFlatFold {
	            case Step.Done(a, e) => {
	              doneOrError = true
	              val it = Done(a, e)
	              log.debug(s"${req.url} consumer received DONE")
	              iterateeP.success(it)
	              it
	            }
	
	            case Step.Cont(k) => {
	              //log.info(s"${req.url} consumer receiving: ${new String(bodyPart.getBodyPartBytes())}")
	              //log.info(s"${req.url} consumer receiving: ${bodyPart.getBodyPartBytes().map("%02x".format(_)).mkString(" ")}")
	              k(Input.El(bodyPart.getBodyPartBytes()))
	            }
	            
	            case Step.Error(e, input) => {
	              doneOrError = true
	              log.debug(s"${req.url} consumer received error")
	              val it = Error(e, input)
	              iterateeP.success(it)
	              it
	            }
	          }
	          
	          STATE.CONTINUE
	        } else {
	          iteratee = null
	          // Must close underlying connection, otherwise async http client will drain the stream
	          log.debug(s"${req.url} doneOrError - closing connection")
	          bodyPart.markUnderlyingConnectionAsClosed()
	          STATE.ABORT
	        }    	    
    	  }

    	  override def onCompleted() = {
    		  log.info(s"${req.url} completed")
    		  Option(iteratee).map(iterateeP.success)
    	  }

    	  override def onThrowable(t: Throwable) = {
    		  iterateeP.failure(t)
    	  }
    }
       
    log.debug(s"connecting to: ${req.url}")
    val futureResponse = Http(req > handler)
    iterateeP.future
  }
}

sealed case class DockerClientV19(dockerHost: String, dockerPort: Int) extends DockerClient {
  final val dockerApiVersion: String = "1.9"
}

sealed case class DockerClientV114(dockerHost: String, dockerPort: Int) extends DockerClient {
  final val dockerApiVersion: String = "1.14"
}


object Docker {

  def apply(): DockerClient = {
    System.getenv("DOCKER_HOST") match {
      case str if (str.nonEmpty) =>
        val uri = com.netaporter.uri.Uri.parse(str)
        val host = uri.host.getOrElse("localhost")
        val port = uri.port.getOrElse(2375)
        Docker.apply(host, port)
      case _ => throw new DockerException {
        override def message: String = "unable to read DOCKER_HOST env"
      }
    }
  }

  def apply(host: String): DockerClient = {
    DockerClientV114(host, 4243)
  }
  
  def apply(host: String, port: Int): DockerClient = {
    DockerClientV114(host, port)
  }
  
}