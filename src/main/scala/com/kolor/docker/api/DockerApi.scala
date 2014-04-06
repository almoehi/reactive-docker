package com.kolor.docker.api

import com.kolor.docker.api.types._
import scala.concurrent.Future
import dispatch._
import Defaults._
import play.api.libs.json.Json
import play.api.libs.json.JsResult
import com.kolor.docker.api._
import com.kolor.docker.api.types._
import com.netaporter.uri.Uri
import play.api.libs.json.Format
import play.api.libs.json._
import play.api.libs.iteratee.Enumerator
import org.joda.time.DateTime
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory
import scala.concurrent.ExecutionContext
import java.io.FileOutputStream
import com.kolor.docker.api.types.DockerStatusMessage

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

trait DockerApi extends DockerContainerApi with DockerImagesApi with DockerApiHelper {
  
  private val log = LoggerFactory.getLogger(this.getClass());

    /*
val params = Map("commit" -> "true")
val headers = Map("Content-type" -> "application/json")
val solr = host("localhost", 8983)
val req = solr / "update" / "json" << a <<? params <:< headers
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

  def dockerInfo()(implicit docker: DockerClient, fmt: Format[DockerInfo]): Future[DockerInfo] = {
    val req = url(Endpoints.dockerInfo.toString).GET
    docker.dockerJsonRequest[DockerInfo](req).map {
      case Right(v) => v
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"docker info request failed", docker, Some(t), Some(req))
    }
  }
  
  def dockerVersion()(implicit docker: DockerClient, fmt: Format[DockerVersion]): Future[DockerVersion] = {
    val req = url(Endpoints.dockerVersion.toString).GET
    docker.dockerJsonRequest[DockerVersion](req).map {
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Right(version) => version
      case Left(t) => throw new DockerRequestException(s"docker version request failed", docker, Some(t), Some(req))
    }
  }
  
  def dockerEvents(since: Option[DateTime] = None)(implicit docker: DockerClient, fmt: Format[DockerStatusMessage], errorFmt: Format[DockerErrorInfo], progressFmt: Format[DockerProgressInfo]): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
    val req = url(Endpoints.dockerEvents(Some(since.map(_.getMillis()).getOrElse(DateTime.now().getMillis() - (100*10)))).toString).GET
    recoverDockerAwareRequest(docker.dockerRequestIteratee(req)(_ => DockerIteratee.statusStream)).flatMap(_.run)
  }
  
  def dockerEventsStreamIteratee[T](consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, fmt: Format[DockerStatusMessage], errorFmt: Format[DockerErrorInfo], progressFmt: Format[DockerProgressInfo]): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.dockerEvents().toString).GET
    recoverDockerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def dockerEventsStream(fn: (Either[DockerErrorInfo,DockerStatusMessage]) => Unit)(implicit docker: DockerClient, fmt: Format[DockerStatusMessage], errorFmt: Format[DockerErrorInfo], progressFmt: Format[DockerProgressInfo]): Future[Unit] = {
    dockerEventsStreamIteratee(DockerEnumeratee.statusStream() &>> Iteratee.foreach(fn)).flatMap(_.run)
  }
  
  def dockerBuildIteratee[T](tarFile: java.io.File, tag: String, verbose: Boolean = false, nocache: Boolean = false)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Iteratee[Array[Byte], T]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.dockerBuild(tag, verbose, nocache).toString).POST
    	case data => url(Endpoints.dockerBuild(tag, verbose, nocache).toString).POST <:< Map("X-Registry-Config" -> data.asBase64Encoded)
    }
    
    recoverDockerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def dockerBuild(tarFile: java.io.File, tag: String, verbose: Boolean = false, nocache: Boolean = false)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
    dockerBuildIteratee(tarFile, tag, verbose, nocache)(DockerIteratee.statusStream).flatMap(_.run)
  }
}

trait DockerContainerApi extends DockerApiHelper {

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
  
  def containerInspect(id: ContainerId)(implicit docker: DockerClient, fmt: Format[ContainerInfo]): Future[ContainerInfo] = {
    val req = url(Endpoints.containerInspect(id).toString).GET
    docker.dockerJsonRequest[ContainerInfo](req).map { 
      case Right(info) => info
      case Left(StatusCode(404)) => throw new NoSuchContainerException(id, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"inspect container request failed", docker, Some(t), Some(req))
    }
  }
  
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
  
  def containerChangelog(id: ContainerId)(implicit docker: DockerClient, fmt: Format[ContainerChangelogRecord]): Future[Seq[ContainerChangelogRecord]] = {
    val req = url(Endpoints.containerChangelog(id).toString).GET
    docker.dockerJsonRequest[Seq[ContainerChangelogRecord]](req).map { 
      case Right(log) => log
      case Left(StatusCode(404)) => throw new NoSuchContainerException(id, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"container changelog request failed", docker, Some(t), Some(req))
    }
  }
  
  def containerExportIteratee[T](id: ContainerId)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.containerExport(id).toString).GET
    implicit val containerId = id
    recoverContainerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def containerExport(id: ContainerId, toFile: java.io.File)(implicit docker: DockerClient): Future[java.io.File] = {
    val os = new FileOutputStream(toFile)
    containerExportIteratee(id)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ =>
    	os.close()
    	toFile
    }
  }
  
  def containerExport(id: ContainerId, os: java.io.OutputStream)(implicit docker: DockerClient): Future[java.io.OutputStream] = {
    containerExportIteratee(id)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ =>
    	os.close()
    	os
    }
  }
  
  def containerStart(id: ContainerId, config: Option[ContainerHostConfiguration] = None)(implicit docker: DockerClient, fmt: Format[ContainerHostConfiguration]): Future[Boolean] = {
    val req = config match {
      	case Some(cfg) => url(Endpoints.containerStart(id).toString).POST << Json.prettyPrint(Json.toJson(cfg)) <:< Map("Content-Type" -> "application/json")
      	case _ => url(Endpoints.containerStart(id).toString).POST << Json.prettyPrint(Json.toJson(ContainerHostConfiguration())) <:< Map("Content-Type" -> "application/json")
    }
    
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to start container $id (StatusCode: ${resp.getStatusCode()}): ${resp.getStatusText()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to start container $id", docker, Some(t), Some(req))
    }
  }
  
  def containerStop(id: ContainerId, timeoutToKill: Int = 60)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerStop(id, timeoutToKill).toString).POST 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to stop container $id (Code ${resp.getStatusCode()}): ${resp.getResponseBody()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to stop container $id", docker, Some(t), Some(req))
    }
  }
  
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
  
  def attachStream[T](id: ContainerId, stdin: Boolean = false, stdout: Boolean = true, stderr: Boolean = false, logs: Boolean = true)(consumer: Iteratee[Array[Byte], T] = Iteratee.ignore)(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.containerAttach(id, true, stdin, stdout, stderr, logs).toString).POST
    docker.dockerRequestIteratee(req)(_ => consumer).recoverWith{
      case DockerResponseCode(400, err) => throw new DockerBadParameterException(s"unable to attach to container $id", docker, req)
      case DockerResponseCode(404, err) => Future.failed(new NoSuchContainerException(id, docker))
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(docker, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker streaming attach to container $id failed (Code: $code): err", docker, Some(t), None))
    }
  }
  
  def attach[T](id: ContainerId, stdin: Boolean = false, stdout: Boolean = true, stderr: Boolean = false, logs: Boolean = true)(consumer: Iteratee[Array[Byte], T] = Iteratee.ignore)(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.containerAttach(id, false, stdin, stdout, stderr, logs).toString).POST
    
    docker.dockerRequestIteratee(req)(_ => consumer).recoverWith{
      case DockerResponseCode(400, err) => throw new DockerBadParameterException(s"unable to attach to container $id", docker, req)
      case DockerResponseCode(404, err) => Future.failed(new NoSuchContainerException(id, docker))
      case DockerResponseCode(500, err) => Future.failed(new DockerInternalServerErrorException(docker, err))
      case t @ DockerResponseCode(code, err) => Future.failed(new DockerRequestException(s"Docker attach to container $id failed (Code: $code): err", docker, Some(t), None))
    }
  }
  
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
  
  def containerRemove(id: ContainerId, withVolumes: Boolean = false)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerRemove(id, withVolumes).toString).DELETE 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 400) => throw new DockerBadParameterException(s"removing container $id failed", docker, req)
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Right(resp) => throw new DockerRequestException(s"unable to remove container $id (Code ${resp.getStatusCode()}): ${resp.getResponseBody()}", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"unable to remove container $id", docker, Some(t), Some(req))
    }
  }
  
  def containerCopyResourceIteratee[T](id: ContainerId, resourcePath: String)(consumer: Iteratee[Array[Byte], T] = Iteratee.ignore)(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val json = Json.obj("Resource" -> resourcePath)
    val req = url(Endpoints.containerCopy(id).toString).POST << Json.prettyPrint(json) <:< Map("Content-Type" -> "application/json")
    implicit val containerId = id
    recoverContainerAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def containerCopyResource(id: ContainerId, resourcePath: String)(implicit docker: DockerClient): Future[List[Either[DockerErrorInfo,DockerStatusMessage]]] = {
	  containerCopyResourceIteratee(id, resourcePath)(DockerIteratee.statusStream).flatMap(_.run)
  }

  def containerCommit(id: ContainerId, repoTag: RepositoryTag, runConfig: Option[ContainerConfiguration] = None)(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[ContainerId] = {
    val cfg = runConfig.map(j => Json.prettyPrint(Json.toJson(j)))
    val req = url(Endpoints.containerCommit(id, repoTag.repo, repoTag.tag, cfg).toString).POST << cfg.getOrElse("{}")
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
  
  def containerCommitWithMessage(id: ContainerId, repoTag: RepositoryTag, withMessage: (String, Option[String]), runConfig: Option[ContainerConfiguration] = None)(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[ContainerId] = {
    val commitMsg = withMessage
    val cfg = runConfig.map(j => Json.prettyPrint(Json.toJson(j)))

    val req = url(Endpoints.containerCommit(id, repoTag.repo, repoTag.tag, cfg, Some(commitMsg._1), commitMsg._2).toString).POST << cfg.getOrElse("{}")
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

trait DockerImagesApi extends DockerApiHelper {
  
  private val log = LoggerFactory.getLogger(this.getClass());

  def images(all: Boolean = false)(implicit docker: DockerClient, fmt: Format[DockerImage]): Future[Seq[DockerImage]] = {
    val req = url(Endpoints.images(all).toString).GET
    docker.dockerJsonRequest[Seq[DockerImage]](req).map { 
      case Right(images) => images
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"list images request failed", docker, Some(t), Some(req))
    }
  }
  
  def imageCreateIteratee[T](repoTag: RepositoryTag, registry: Option[String] = None, fromSrc: Option[String] = None)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Iteratee[Array[Byte], T]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.imageCreate(repoTag.repo, fromSrc, Some(repoTag.repo), repoTag.tag, registry).toString).POST
    	case data => url(Endpoints.imageCreate(repoTag.repo, fromSrc, Some(repoTag.repo), repoTag.tag, registry).toString).POST <:< Map("X-Registry-Auth" -> data.asBase64Encoded)
    }
    implicit val image = repoTag.repo
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def imageCreate(repoTag: RepositoryTag, registry: Option[String] = None, fromSrc: Option[String] = None)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
    imageCreateIteratee(repoTag, registry, fromSrc)(DockerIteratee.statusStream).flatMap(_.run)
  }
  
  def imageInsertResourceIteratee[T](image: String, imageTargetPath: String, sourceFileUrl: java.net.URI)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.imageInsert(image, imageTargetPath, sourceFileUrl).toString).POST
    implicit val img = image
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def imageInsertResource(image: String, imageTargetPath: String, sourceFileUrl: java.net.URI)(implicit docker: DockerClient): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
	  imageInsertResourceIteratee(image, imageTargetPath, sourceFileUrl)(DockerIteratee.statusStream).flatMap(_.run)
  }
  
  def imageInspect(image: String)(implicit docker: DockerClient, fmt: Format[DockerImageInfo]): Future[DockerImageInfo] = {
    val req = url(Endpoints.imageInspect(image).toString).GET
    docker.dockerJsonRequest[DockerImageInfo](req).map { 
      case Right(info) => info
      case Left(StatusCode(404)) => throw new NoSuchImageException(image, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"inspect image $image request failed", docker, Some(t), Some(req))
    }
  }
  
  def imageHistory(image: String)(implicit docker: DockerClient, fmt: Format[DockerImageHistoryInfo]): Future[Seq[DockerImageHistoryInfo]] = {
    val req = url(Endpoints.imageHistory(image).toString).GET
    
    docker.dockerJsonRequest[Seq[DockerImageHistoryInfo]](req).map { 
      case Right(info) => info
      case Left(StatusCode(404)) => throw new NoSuchImageException(image, docker)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"inspect image $image request failed", docker, Some(t), Some(req))
    }
  }
  
  def imagePushIteratee[T](image: String, registry: Option[String] = None)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Iteratee[Array[Byte], T]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.imagePush(image, registry).toString).POST
    	case data => url(Endpoints.imagePush(image, registry).toString).POST <:< Map("X-Registry-Auth" -> data.asBase64Encoded)
    }
    implicit val img = image
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def imagePush(image: String, registry: Option[String] = None)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[List[Either[DockerErrorInfo, DockerStatusMessage]]] = {
	  imagePushIteratee(image, registry)(DockerIteratee.statusStream).flatMap(_.run)
  }
  
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
  
  def imageRemove(image: String)(implicit docker: DockerClient): Future[Seq[(String,String)]] = {
    val req = url(Endpoints.imageRemove(image).toString).DELETE
    
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
  
  def imageSearch(query: String)(implicit docker: DockerClient, fmt: Format[DockerImageSearchResult]): Future[Seq[DockerImageSearchResult]] = {
    val req = url(Endpoints.imageSearch(query).toString).GET
    
    docker.dockerJsonRequest[Seq[DockerImageSearchResult]](req).map { 
      case Right(results) => results
      case Left(StatusCode(500)) => throw new DockerRequestException(s"searching image (term: $query) request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"searching image (term: $query) request failed", docker, Some(t), Some(req))
    }
  }
  
  def imageExport[T](image: String)(consumer: Iteratee[Array[Byte], T])(implicit docker: DockerClient): Future[Iteratee[Array[Byte], T]] = {
    val req = url(Endpoints.imageExport(image).toString).GET
    implicit val img = image
    recoverImageAwareRequest(docker.dockerRequestIteratee(req)(_ => consumer))
  }
  
  def imageExport(image: String, toFile: java.io.File)(implicit docker: DockerClient): Future[java.io.File] = {
	  val os = new FileOutputStream(toFile)
	  imageExport(image)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ => 
	  	os.close()
	  	toFile
	  }
  }
  
  def imageExport(image: String, os: java.io.OutputStream)(implicit docker: DockerClient): Future[java.io.OutputStream] = {
	  imageExport(image)(Iteratee.foreach[Array[Byte]](d => os.write(d))).flatMap(_.run).map{_ => 
	  	os.close()
	  	os
	  }
  }
  
  def imageImport(tarFile: java.io.File)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.imagesLoad.toString).POST <<< tarFile <:< Map("Content-Type" -> "application/x-tar")
    docker.dockerRequest(req).map {
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 500) => throw new DockerRequestException(s"importing image from $tarFile request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"importing image from $tarFile request failed", docker, Some(t), Some(req))
    }
  }
}