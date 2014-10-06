package com.kolor.docker.api

import dispatch._
import Defaults._
import com.kolor.docker.api.types._
import play.api.libs.json._
import org.joda.time.DateTime
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory
import java.io._
import scala.concurrent.Future
import com.ning.http.client.generators.InputStreamBodyGenerator
import com.kolor.docker.api.types.DockerVersion
import play.api.libs.json.JsArray
import com.kolor.docker.api.types.DockerStatusMessage
import scala.Some
import dispatch.StatusCode
import com.kolor.docker.api.types.ContainerInfo
import com.kolor.docker.api.types.ContainerChangelogRecord
import com.kolor.docker.api.types.Container
import com.kolor.docker.api.types.DockerImageInfo
import com.kolor.docker.api.types.ContainerHostConfiguration
import com.kolor.docker.api.types.DockerImageSearchResult
import com.kolor.docker.api.types.DockerInfo
import com.kolor.docker.api.types.DockerImage
import com.kolor.docker.api.types.DockerErrorInfo
import com.kolor.docker.api.types.DockerProgressInfo
import com.kolor.docker.api.types.DockerAuthCredentials
import com.kolor.docker.api.types.DockerImageHistoryInfo
import play.api.libs.json.JsObject
import com.kolor.docker.api.types.ContainerConfiguration

/**
 * helper trait
 * contains common utility methods
 */
sealed trait DockerApiHelper {
  
  def recoverContainerAwareRequest[A](it: Future[Iteratee[Array[Byte], A]])(implicit client: DockerClient, id: ContainerId) = {
    it.recoverWith{
      case DockerResponseCode(404, err) => Future.failed(new NoSuchContainerException(id, client))
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(client, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker request failed (Code: $code): err", client, Some(t), None))
    }
  }
  
  def recoverImageAwareRequest[A](it: Future[Iteratee[Array[Byte], A]])(implicit client: DockerClient, image: String) = {
    it.recoverWith{
      case DockerResponseCode(404, err) => Future.failed(new NoSuchImageException(image, client))
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(client, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker request failed (Code: $code): err", client, Some(t), None))
    }
  }
  
  def recoverDockerAwareRequest[A](it: Future[Iteratee[Array[Byte], A]])(implicit client: DockerClient) = {
    it.recoverWith{
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(client, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker request failed (Code: $code): err", client, Some(t), None))
    }
  }
}

/**
 * DockerApi trait
 * is composed of ContainerApi and ImagesApi
 */
trait DockerApi extends DockerContainerApi with DockerImagesApi with DockerApiHelper {
  
  private val log = LoggerFactory.getLogger(this.getClass());

    /*
val params = Map("commit" -> "true")
val headers = Map("Content-type" -> "application/json")
val solr = host("localhost", 8983)
val req = solr / "update" / "json" << a <<? params <:< headers
     */
  
  /**
   * auth against docker using
   */
  def dockerAuth(authInfo: DockerAuth)(implicit docker: DockerClient, fmt: Format[DockerAuth]): Future[Boolean] = {
    val req = authInfo match {
      case data:DockerAuthCredentials => url(Endpoints.dockerAuth.toString).POST << Json.prettyPrint(Json.toJson(data)) <:< Map("Content-Type" -> "application/json")
      case _ => url(Endpoints.dockerAuth.toString).POST <:< Map("Content-Type" -> "application/json")
    }
    
    docker.dockerRequest(req).map{
      case Right(resp) if resp.getStatusCode() == 200 => true
      case Right(resp) if resp.getStatusCode() == 201 => true
      case Right(resp) if resp.getStatusCode() == 500 => false // throw new DockerInternalServerErrorException(docker)
      case Right(resp) => false
      case Left(t) => throw new DockerRequestException(s"docker auth request failed", docker, Some(t), Some(req))
    }
  }

  /**
   * retrieve docker info
   */
  def dockerInfo()(implicit docker: DockerClient, fmt: Format[DockerInfo]): Future[DockerInfo] = {
    val req = url(Endpoints.dockerInfo.toString).GET
    docker.dockerJsonRequest[DockerInfo](req).map {
      case Right(v) => v
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"docker info request failed", docker, Some(t), Some(req))
    }
  }
  
  /**
   * PING docker host
   */
  def dockerPing()(implicit docker: DockerClient): Future[Boolean] = {
	  val req = url(Endpoints.dockerPing.toString).GET
	  docker.httpRequest(req).map {
	    case Right(str) if (str.toLowerCase().equalsIgnoreCase("ok")) => true
	    case Right(str) => false
	    case Left(t) => throw new DockerRequestException(s"docker ping request failed", docker, Some(t), Some(req))
	  }
  }

  
  /**
   * retrieve docker version
   */
  def dockerVersion()(implicit docker: DockerClient, fmt: Format[DockerVersion]): Future[DockerVersion] = {
    val req = url(Endpoints.dockerVersion.toString).GET
    docker.dockerJsonRequest[DockerVersion](req).map {
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Right(version) => version
      case Left(t) => throw new DockerRequestException(s"docker version request failed", docker, Some(t), Some(req))
    }
  }
  
  /**
   * retrieve list of docker events since given timestamp
   * timestamp defaults to past 10 seconds
   */
  def dockerEvents(since: Option[DateTime] = None, until: Option[DateTime] = None)(implicit docker: DockerClient, fmt: Format[DockerStatusMessage], errorFmt: Format[DockerErrorInfo], progressFmt: Format[DockerProgressInfo]): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
    val req = url(Endpoints.dockerEvents(Some(since.map(_.getMillis() / 1000).getOrElse(DateTime.now().getMillis()/1000 - (100*10))), until.map(_.getMillis() / 1000)).toString).GET
    recoverDockerAwareRequest(docker.dockerRequestIteratee(req)(_ => DockerIteratee.statusStream)).flatMap(_.run)
  }
  
  /**
   * attach to dockers event endpoint
   * enabled to receive events in realtime by the given consumer iteratee.
   * You can use @{DockerEnumeratee.statusStream} to transform the byte stream into a stream of Either[DockerErrorInfo,DockerStatusMessage]
   */
  def dockerEventsStreamIteratee[T](consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, fmt: Format[DockerStatusMessage], errorFmt: Format[DockerErrorInfo], progressFmt: Format[DockerProgressInfo]): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.dockerEvents().toString).GET
    recoverDockerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  /**
   * attach to dockers event endpoint
   * retrieves events in realtime and execute a callback on every received Either[DockerErrorInfo,DockerStatusMessage] event
   */
  def dockerEventsStream(fn: (Either[DockerErrorInfo,DockerStatusMessage]) => Unit)(implicit docker: DockerClient, fmt: Format[DockerStatusMessage], errorFmt: Format[DockerErrorInfo], progressFmt: Format[DockerProgressInfo]): Future[Unit] = {
    dockerEventsStreamIteratee(DockerEnumeratee.statusStream() &>> Iteratee.foreach(fn)).flatMap(_.run)
  }
  
  /**
   * build image from dockerfile
   * allows realtime processing of the response by given iteratee
   */
  def dockerBuildIteratee[T](tarFile: java.io.File, tag: String, verbose: Boolean = false, nocache: Boolean = false, forceRm: Boolean = false)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Iteratee[Array[Byte], T]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.dockerBuild(tag, verbose, nocache, forceRm).toString).POST
    	case data => url(Endpoints.dockerBuild(tag, verbose, nocache).toString).POST <:< Map("X-Registry-Config" -> data.asBase64Encoded)
    }
    
    recoverDockerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  /**
   * build image from dockerfile
   * collects and aggregates all docker messages into a list on completion
   */
  def dockerBuild(tarFile: java.io.File, tag: String, verbose: Boolean = false, nocache: Boolean = false, forceRm: Boolean = false)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
    dockerBuildIteratee(tarFile, tag, verbose, nocache, forceRm)(DockerIteratee.statusStream).flatMap(_.run)
  }
}


/**
 * ContainerApi trait
 * contains all container-related api operations
 */
trait DockerContainerApi extends DockerApiHelper {

  private val log = LoggerFactory.getLogger(this.getClass());

  /**
   * list containers
   */
  def containers(all:Boolean = true, limit: Option[Int] = None, sinceId: Option[String] = None, beforeId: Option[String] = None, showSize: Boolean = true)(implicit docker: DockerClient, fmt: Format[Container]): Future[Seq[Container]] = {
    val req = url(Endpoints.containers(all, limit, sinceId, beforeId, showSize).toString).GET
    docker.dockerJsonRequest[Seq[Container]](req).map {
      case Left(StatusCode(400)) => throw new DockerBadParameterException("list containers - bad parameter", docker, req)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(StatusCode(302)) => throw new DockerRequestException(s"list containers failed (Code: 302)", docker, None, Some(req))
      case Right(v) => v 
      case Left(t) => throw new DockerRequestException(s"list containers request failed", docker, Some(t), Some(req))
    }
  }
  
  /**
   * create a new container from given image
   * returns the new containerId and a sequence of warnings
   */
  def containerCreate(image: String, config: ContainerConfiguration, name: Option[String] = None)(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[(ContainerId, Seq[String])] = {
    val cfg = config.copy(image = Some(image))
    val req = url(Endpoints.containerCreate(name).toString).POST << Json.prettyPrint(Json.toJson(cfg)) <:< Map("Content-Type" -> "application/json")
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val id = json.flatMap(j => (j \ "Id").asOpt[String]).map(ContainerId(_)).getOrElse(ContainerId.emptyId)
        throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 406 => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val id = json.flatMap(j => (j \ "Id").asOpt[String]).map(ContainerId(_)).getOrElse(ContainerId.emptyId)
        throw new ContainerNotRunningException(id, docker)
      case Right(resp) if resp.getStatusCode() == 409 => throw new DockerConflictException(s"create container request failed: ${resp.getResponseBody()}", docker) 
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if (Seq(200, 201).contains(resp.getStatusCode())) => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val id = json.flatMap(j => (j \ "Id").asOpt[String]).map(ContainerId(_)).getOrElse(ContainerId.emptyId)
        val warnings: Seq[String] = json.flatMap(j => (j \ "Warnings").asOpt[Seq[String]]).getOrElse(Seq.empty)
        (id, warnings)
      case Right(resp) => throw new DockerRequestException(s"create container request failed (response code ${resp.getStatusCode()})", docker, None, Some(req)) 
      case Left(t) => throw new DockerRequestException(s"create container request failed", docker, Some(t), Some(req))
    }
  }
  
  /**
   * inspect a container
   */
  def containerInspect(id: ContainerId)(implicit docker: DockerClient, fmt: Format[ContainerInfo]): Future[ContainerInfo] = {
    val req = url(Endpoints.containerInspect(id).toString).GET
    docker.dockerJsonRequest[ContainerInfo](req).map { 
      case Right(info) => info
      case Left(StatusCode(404)) => throw new NoSuchContainerException(id, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"inspect container request failed", docker, Some(t), Some(req))
    }
  }
  
  /**
   * list running processes of container
   */
  def containerProcesses(id: ContainerId, psArgs: Option[String] = None)(implicit docker: DockerClient): Future[(Seq[String], Seq[Seq[String]])] = {
    val req = url(Endpoints.containerProcesses(id, psArgs).toString).GET
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val columns: Option[Seq[String]] = json.flatMap(j => (j \ "Titles").asOpt[Seq[String]])
        val rows: Option[JsArray] = json.flatMap(j => (j \ "Processes").asOpt[JsArray])
        
        val processes: Seq[Seq[String]] = rows.flatMap{items => 
          items.asOpt[Seq[Seq[String]]]
        }.getOrElse(Seq.empty)
        
        (columns.getOrElse(Seq.empty), processes)
      case Left(t) => throw new DockerRequestException(s"container processes request failed", docker, Some(t), Some(req))
    }
  }
  
  /**
   * retrieve container's changelog
   */
  def containerChangelog(id: ContainerId)(implicit docker: DockerClient, fmt: Format[ContainerChangelogRecord]): Future[Seq[ContainerChangelogRecord]] = {
    val req = url(Endpoints.containerChangelog(id).toString).GET
    docker.dockerJsonRequest[Seq[ContainerChangelogRecord]](req).map { 
      case Right(log) => log
      case Left(StatusCode(404)) => throw new NoSuchContainerException(id, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"container changelog request failed", docker, Some(t), Some(req))
    }
  }
  
  /**
   * export container as tarball
   * allows realtime response processing by provided consumer iteratee
   */
  def containerExportIteratee[T](id: ContainerId)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.containerExport(id).toString).GET
    implicit val containerId = id
    recoverContainerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  /**
   * export container as tarball to given file
   */
  def containerExport(id: ContainerId, toFile: java.io.File)(implicit docker: DockerClient): Future[java.io.File] = {
    val os = new FileOutputStream(toFile)
    containerExportIteratee(id)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ =>
    	os.close()
    	toFile
    }
  }
  
  /**
   * export container as tarball to given outputstream
   */
  def containerExport(id: ContainerId, os: java.io.OutputStream)(implicit docker: DockerClient): Future[java.io.OutputStream] = {
    containerExportIteratee(id)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ =>
    	os.close()
    	os
    }
  }
  
  /**
   * start a container
   */
  def containerStart(id: ContainerId, config: Option[ContainerHostConfiguration] = None)(implicit docker: DockerClient, fmt: Format[ContainerHostConfiguration]): Future[Boolean] = {
    val req = config match {
      	case Some(cfg) => url(Endpoints.containerStart(id).toString).POST << Json.prettyPrint(Json.toJson(cfg)) <:< Map("Content-Type" -> "application/json")
      	case _ => url(Endpoints.containerStart(id).toString).POST << Json.prettyPrint(Json.toJson(ContainerHostConfiguration())) <:< Map("Content-Type" -> "application/json")
    }
    
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 304) => true		// new with 1.13 => container status was not modified
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to start container $id (StatusCode: ${resp.getStatusCode()}): ${resp.getStatusText()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to start container $id", docker, Some(t), Some(req))
    }
  }
  
  /**
   * stop a container
   * note: throws an exception is container is not running
   */
  def containerStop(id: ContainerId, timeoutToKill: Int = 60)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerStop(id, timeoutToKill).toString).POST 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 304) => true		// new with 1.13 => container status was not modified
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to stop container $id (Code ${resp.getStatusCode()}): ${resp.getResponseBody()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to stop container $id", docker, Some(t), Some(req))
    }
  }
  
  /**
   * restart container
   */
  def containerRestart(id: ContainerId, timeoutToKill: Int = 60)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerRestart(id, timeoutToKill).toString).POST 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to restart container $id (StatusCode: ${resp.getStatusCode()}): ${resp.getStatusText()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to restart container $id", docker, Some(t), Some(req))
    }
  }
  
  /**
   * kill container
   */
  def containerKill(id: ContainerId)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerKill(id).toString).POST 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to kill container $id (StatusCode: ${resp.getStatusCode()}): ${resp.getStatusText()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to kill container $id", docker, Some(t), Some(req))
    }
  }

  /**
   * attach to a containers stdout, stderr, logs and stdin channel and stream their contents
   */
  def attachStream[T](id: ContainerId, stdout: Boolean = true, stderr: Boolean = false, logs: Boolean = false, stdin: Option[Enumerator[Array[Byte]]] = None)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    
     // TODO: use containerLogs if logs == true
    
    val req = stdin match {
      case Some(en) =>
        // transform stdin enumerator into  an inputstream and attach to request
        val os = new PipedOutputStream()
        val is = new PipedInputStream(os)

        (en |>>> Iteratee.foreach{b =>
          //println(s"STDIN:Container.$id> ${new String(b)}")
          os.write(b)
        }).map{_ =>
          os.close()
          is.close()
        }
        url(Endpoints.containerAttach(id, true, true, stdout, stderr, logs).toString)
          .POST
          .subject.underlying(_.setBody(new InputStreamBodyGenerator(is)))

      case _ => url(Endpoints.containerAttach(id, true, false, stdout, stderr, logs).toString).POST
    }

    docker.dockerRequestIteratee(req)(_ => consumer).recoverWith{
      case DockerResponseCode(400, err) => throw new DockerBadParameterException(s"unable to attach to container $id", docker, req)
      case DockerResponseCode(404, err) => Future.failed(new NoSuchContainerException(id, docker))
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(docker, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker streaming attach to container $id failed (Code: $code): err", docker, Some(t), None))
    }
  }

  /**
   * attach given enumerator to stdin only of a container
   * returns a future which will be completed once the enumerator is done / detached
   */
  def attachStdin(id: ContainerId, en: Enumerator[Array[Byte]])(implicit docker: DockerClient): Future[Unit] = {
    // TODO: use containerLogs
    val os = new PipedOutputStream()
    val is = new PipedInputStream(os)

    (en |>>> Iteratee.foreach{b =>
      os.write(b)
    }).map{_ =>
      os.close()
      is.close()
    }

    val req = url(Endpoints.containerAttach(id, true, true, false, false, false).toString)
              .POST
              .subject.underlying(_.setBody(new InputStreamBodyGenerator(is)))

    val consumer = Iteratee.ignore[Array[Byte]]
    docker.dockerRequestIteratee(req)(_ => consumer).recoverWith{
      case DockerResponseCode(400, err) => throw new DockerBadParameterException(s"unable to attach to container $id", docker, req)
      case DockerResponseCode(404, err) => Future.failed(new NoSuchContainerException(id, docker))
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(docker, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker streaming attach to container $id failed (Code: $code): err", docker, Some(t), None))
    }.flatMap(_.run).map(_ => Unit)
  }

  /**
   * attach to stdout, stderr and/or logs channel
   * returns data immediately - no streaming
   */
  def attach[T](id: ContainerId, stdout: Boolean = true, stderr: Boolean = false, logs: Boolean = false)(consumer: Iteratee[Array[Byte], T] = Iteratee.ignore)(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    // TODO: use containerLogs if logs == true
    
    val req = url(Endpoints.containerAttach(id, false, false, stdout, stderr, logs).toString).POST
    
    docker.dockerRequestIteratee(req)(_ => consumer).recoverWith{
      case DockerResponseCode(400, err) => throw new DockerBadParameterException(s"unable to attach to container $id", docker, req)
      case DockerResponseCode(404, err) => Future.failed(new NoSuchContainerException(id, docker))
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(docker, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker attach to container $id failed (Code: $code): err", docker, Some(t), None))
    }
  }

  /**
   * wait for container to terminate an execute given callback on exit code
   */
  def containerWait[T](id: ContainerId)(action: Int => T)(implicit docker: DockerClient): Future[T] = {
    val req = url(Endpoints.containerWait(id).toString).POST
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val statusCode: Option[Int] = json.flatMap(j => (j \ "StatusCode").asOpt[Int])
        action(statusCode.getOrElse(-1))
    }
  }
  
  /**
   * remove container
   */
  def containerRemove(id: ContainerId, withVolumes: Boolean = false, force: Boolean = false)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerRemove(id, withVolumes, force).toString).DELETE 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 400) => throw new DockerBadParameterException(s"removing container $id failed", docker, req)
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to remove container $id (Code ${resp.getStatusCode()}): ${resp.getResponseBody()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to remove container $id", docker, Some(t), Some(req))
    }
  }
  
  /**
   * copy a resource from container
   * allows realtime processing of response by given iterratee
   */
  def containerCopyResourceIteratee[T](id: ContainerId, resourcePath: String)(consumer: Iteratee[Array[Byte], T] = Iteratee.ignore)(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val json = Json.obj("Resource" -> resourcePath)
    val req = url(Endpoints.containerCopy(id).toString).POST << Json.prettyPrint(json) <:< Map("Content-Type" -> "application/json")
    implicit val containerId = id
    recoverContainerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  /**
   * copy resource from container to targetFile
   */
  def containerCopyResource(id: ContainerId, resourcePath: String, toFile: java.io.File)(implicit docker: DockerClient): Future[java.io.File] = {
	  val os = new FileOutputStream(toFile)
    containerCopyResourceIteratee(id, resourcePath)(Iteratee.foreach(b => os.write(b))).flatMap(_.run).map{_ =>
      os.close()
      toFile
    }
  }

  /**
   * copy resource from container to outputstream
   */
  def containerCopyResource(id: ContainerId, resourcePath: String, os: java.io.OutputStream)(implicit docker: DockerClient): Future[java.io.OutputStream] = {
    containerCopyResourceIteratee(id, resourcePath)(Iteratee.foreach(b => os.write(b))).flatMap(_.run).map{_ =>
      os.close()
      os
    }
  }

  /**
   * commit a container to repoTag without commit message
   */
  def containerCommit(id: ContainerId, repoTag: RepositoryTag, runConfig: Option[ContainerConfiguration] = None, pause: Boolean = true)(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[ContainerId] = {
    val cfg = runConfig.map(j => Json.prettyPrint(Json.toJson(j)))
    val req = url(Endpoints.containerCommit(id, repoTag.repo, repoTag.tag, cfg, None, None, pause).toString).POST << cfg.getOrElse("{}")
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if (Seq(200, 201).contains(resp.getStatusCode())) => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val newId: ContainerId = json.flatMap(j => (j \ "Id").asOpt[String]).map(ContainerId(_)).getOrElse(ContainerId.emptyId)
        newId
      case Left(t) => throw new DockerRequestException(s"commit container $id (tag: ${repoTag.toString}) failed", docker, Some(t), Some(req))
    }
  }

  /**
   * commit a container to repoTag with comment and author
   */
  def containerCommitWithMessage(id: ContainerId, repoTag: RepositoryTag, withMessageAndAuthor: (String, Option[String]), runConfig: Option[ContainerConfiguration] = None, pause: Boolean = true)(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[ContainerId] = {
    val commitMsg = withMessageAndAuthor
    val cfg = runConfig.map(j => Json.prettyPrint(Json.toJson(j)))

    val req = url(Endpoints.containerCommit(id, repoTag.repo, repoTag.tag, cfg, Some(commitMsg._1), commitMsg._2, pause).toString).POST << cfg.getOrElse("{}")
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if (Seq(200, 201).contains(resp.getStatusCode())) => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val newId: ContainerId = json.flatMap(j => (j \ "Id").asOpt[String]).map(ContainerId(_)).getOrElse(ContainerId.emptyId)
        newId
      case Left(t) => throw new DockerRequestException(s"commit container $id failed", docker, Some(t), Some(req))
    }
  }
  
}

/**
 * ImagesApi trait
 * contains all images related api operations
 */
trait DockerImagesApi extends DockerApiHelper {
  
  private val log = LoggerFactory.getLogger(this.getClass());

  /**
   * list images
   */
  def images(all: Boolean = false)(implicit docker: DockerClient, fmt: Format[DockerImage]): Future[Seq[DockerImage]] = {
    val req = url(Endpoints.images(all).toString).GET
    docker.dockerJsonRequest[Seq[DockerImage]](req).map { 
      case Right(images) => images
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"list images request failed", docker, Some(t), Some(req))
    }
  }

  /**
   * create or pull an image from registry
   * allows realtime processing of response by given consumer iteratee
   */
  def imageCreateIteratee[T](repoTag: RepositoryTag, registry: Option[String] = None, fromSrc: Option[String] = None)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Iteratee[Array[Byte], T]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.imageCreate(repoTag.repo, fromSrc, Some(repoTag.repo), repoTag.tag, registry).toString).POST
    	case data => url(Endpoints.imageCreate(repoTag.repo, fromSrc, Some(repoTag.repo), repoTag.tag, registry).toString).POST <:< Map("X-Registry-Auth" -> data.asBase64Encoded)
    }
    implicit val image = repoTag.repo
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }

  /**
   * create or pull an image from registry
   * collects and returns list of messages/errors on completion
   */
  def imageCreate(repoTag: RepositoryTag, registry: Option[String] = None, fromSrc: Option[String] = None)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
    imageCreateIteratee(repoTag, registry, fromSrc)(DockerIteratee.statusStream).flatMap(_.run)
  }

  /**
   * insert a resource into image from remote location
   * allows realtime processing of reponse by given consumer iteratee
   * removed with API 1.12
   */
  def imageInsertResourceIteratee[T](image: String, imageTargetPath: String, sourceFileUrl: java.net.URI)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.imageInsert(image, imageTargetPath, sourceFileUrl).toString).POST
    implicit val img = image
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }

  /**
   * insert a resource into image from remote location
   * collects and returns list of messages/errors on completion
   * removed with API 1.12
   */
  def imageInsertResource(image: String, imageTargetPath: String, sourceFileUrl: java.net.URI)(implicit docker: DockerClient): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
	  imageInsertResourceIteratee(image, imageTargetPath, sourceFileUrl)(DockerIteratee.statusStream).flatMap(_.run)
  }

  /**
   * inspect an image
   */
  def imageInspect(image: String)(implicit docker: DockerClient, fmt: Format[DockerImageInfo]): Future[DockerImageInfo] = {
    val req = url(Endpoints.imageInspect(image).toString).GET
    docker.dockerJsonRequest[DockerImageInfo](req).map { 
      case Right(info) => info
      case Left(StatusCode(404)) => throw new NoSuchImageException(image, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"inspect image $image request failed", docker, Some(t), Some(req))
    }
  }

  /**
   * retrieve image history / changes
   */
  def imageHistory(image: String)(implicit docker: DockerClient, fmt: Format[DockerImageHistoryInfo]): Future[Seq[DockerImageHistoryInfo]] = {
    val req = url(Endpoints.imageHistory(image).toString).GET
    
    docker.dockerJsonRequest[Seq[DockerImageHistoryInfo]](req).map { 
      case Right(info) => info
      case Left(StatusCode(404)) => throw new NoSuchImageException(image, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"inspect image $image request failed", docker, Some(t), Some(req))
    }
  }

  /**
   * push image to registry
   * allows realtime processing of response using given consumer iteratee
   */
  def imagePushIteratee[T](image: String, registry: Option[String] = None)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Iteratee[Array[Byte], T]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.imagePush(image, registry).toString).POST
    	case data => url(Endpoints.imagePush(image, registry).toString).POST <:< Map("X-Registry-Auth" -> data.asBase64Encoded)
    }
    implicit val img = image
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }

  /**
   * push image to registry
   * collects and returns list of messages/errors on completion
   */
  def imagePush(image: String, registry: Option[String] = None)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
	  imagePushIteratee(image, registry)(DockerIteratee.statusStream).flatMap(_.run)
  }

  /**
   * tag image into a repository
   */
  def imageTag(image: String, repo: String, force: Boolean = false)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.imageTag(image, repo, force).toString).POST
    
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 201) => true
      case Right(resp) if (resp.getStatusCode() == 400) => throw new DockerBadParameterException(s"tagging image $image into $repo failed", docker, req)
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchImageException(image, docker)
      case Right(resp) if (resp.getStatusCode() == 409) => throw new DockerConflictException(s"conflict while tagging $image into $repo: ${resp.getResponseBody()}", docker)
      case Right(resp) if (resp.getStatusCode() == 500) => throw new DockerRequestException(s"tagging image $image into $repo request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"tagging image $image into $repo request failed", docker, Some(t), Some(req))
    }
  }

  /**
   * remove image
   */
  def imageRemove(image: String, force: Boolean = false, noPrune: Boolean = false)(implicit docker: DockerClient): Future[Seq[(String,String)]] = {
    val req = url(Endpoints.imageRemove(image, force, noPrune).toString).DELETE
    
    docker.dockerJsonRequest[Seq[JsObject]](req).map { 
      case Right(output) => output.map{obj => 
        (obj.fields.head._1, obj.fields.head._2.asOpt[String].getOrElse(""))
      }
      case Left(StatusCode(404)) => throw new NoSuchImageException(image, docker)
      case Left(StatusCode(409)) => throw new DockerConflictException(s"conflict while removing image $image", docker)
      case Left(StatusCode(500)) => throw new DockerRequestException(s"removing image $image request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"removing image $image request failed", docker, Some(t), Some(req))
    }
  }

  /**
   * search for available images
   */
  def imageSearch(query: String)(implicit docker: DockerClient, fmt: Format[DockerImageSearchResult]): Future[Seq[DockerImageSearchResult]] = {
    val req = url(Endpoints.imageSearch(query).toString).GET
    
    docker.dockerJsonRequest[Seq[DockerImageSearchResult]](req).map { 
      case Right(results) => results
      case Left(StatusCode(500)) => throw new DockerRequestException(s"searching image (term: $query) request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"searching image (term: $query) request failed", docker, Some(t), Some(req))
    }
  }

  /**
   * export image as tarball
   * allows realtime processing of response using given consumer iteratee
   */
  def imageExport[T](image: String)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.imageExport(image).toString).GET
    implicit val img = image
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }

  /**
   * export image as tarball into target file
   */
  def imageExport(image: String, toFile: java.io.File)(implicit docker: DockerClient): Future[java.io.File] = {
	  val os = new FileOutputStream(toFile)
	  imageExport(image)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ => 
	  	os.close()
	  	toFile
	  }
  }

  /**
   * export image as tarball into outputstream
   */
  def imageExport(image: String, os: java.io.OutputStream)(implicit docker: DockerClient): Future[java.io.OutputStream] = {
	  imageExport(image)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ => 
	  	os.close()
	  	os
	  }
  }

  /**
   * import images from tarball containing 1 or more docker images
   */
  def imageImport(tarFile: java.io.File)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.imagesLoad.toString).POST <<< tarFile <:< Map("Content-Type" -> "application/x-tar")
    docker.dockerRequest(req).map {
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 500) => throw new DockerRequestException(s"importing image from $tarFile request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"importing image from $tarFile request failed", docker, Some(t), Some(req))
    }
  }
}