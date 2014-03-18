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

trait DockerApi extends DockerContainerApi with DockerImagesApi {
  
	protected val log = LoggerFactory.getLogger(this.getClass());

    
    /*
val params = Map("commit" -> "true")
val headers = Map("Content-type" -> "application/json")

val solr = host("localhost", 8983)

val req = solr / "update" / "json" << a <<? params <:< headers

     */
  
  def dockerAuth(authInfo: DockerAuth)(implicit docker: DockerClient): Future[Boolean] = {
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

  def dockerInfo()(implicit docker: DockerClient): Future[DockerInfo] = {
    val req = url(Endpoints.dockerInfo.toString).GET
    docker.dockerJsonRequest[DockerInfo](req).map {
      case Right(v) => v
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"docker info request failed", docker, Some(t), Some(req))
    }
  }
  
  
  def dockerVersion()(implicit docker: DockerClient): Future[DockerVersion] = {
    val req = url(Endpoints.dockerVersion.toString).GET
    docker.dockerJsonRequest[DockerVersion](req).map {
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Right(version) => version
      case Left(t) => throw new DockerRequestException(s"docker version request failed", docker, Some(t), Some(req))
    }
  }
  
  def dockerEvents(since: Option[DateTime] = None)(implicit docker: DockerClient, fmt: Format[DockerEvent]): Future[Seq[DockerEvent]] = {
    val req = url(Endpoints.dockerEvents(since.map(_.getMillis()).getOrElse(DateTime.now().getMillis())).toString).GET
    docker.dockerRequestStream(req).map {
      case Right(enumerator) => 
        //val (iteratee, enumerator) = Concurrent.joined[Array[Byte]]
        /*
        enumerator &>
          play.extras.iteratees.Encoding.decode() &>
          play.extras.iteratees.JsonEnumeratees.jsArray &>
		  Enumeratee.map[JsValue](x => Json.re) &>
		  Enumeratee.collect[JsResult[DockerEvent]] { case JsSuccess(value, _) => value } &>
		  Enumeratee.map[DockerEvent](Seq(_))

  		*/
        Seq.empty
      case Left(StatusCode(500)) => throw new DockerRequestException(s"docker events (since $since) request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"docker events (since $since) request failed", docker, Some(t), Some(req))
    }
  }
  
  def dockerBuild(tarFile: java.io.File, tag: String, verbose: Boolean = false, nocache: Boolean = false)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Seq[JsObject]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.dockerBuild(tag, verbose, nocache).toString).POST
    	case data => url(Endpoints.dockerBuild(tag, verbose, nocache).toString).POST <:< Map("X-Registry-Config" -> data.asBase64Encoded)
    }
    
    docker.dockerRequestStream(req).map { 
      case Right(en) => 
        // TODO: parse en to create tuples from json {"type":"body"} -> (type, body)
        Seq.empty
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"docker build (from $tarFile) request failed", docker, Some(t), Some(req))
    }
  }
}

trait DockerContainerApi {

  def containers(all:Boolean = true, limit: Option[Int] = None, sinceId: Option[String] = None, beforeId: Option[String] = None, showSize: Boolean = true)(implicit docker: DockerClient): Future[Seq[Container]] = {
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
  def containerCreate(image: String, config: ContainerConfiguration, name: Option[String])(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[(ContainerId, Seq[String])] = {
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
  
  def inspectContainer(id: ContainerId)(implicit docker: DockerClient, fmt: Format[ContainerInfo]): Future[ContainerInfo] = {
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
  
  def containerExport(id: ContainerId)(implicit docker: DockerClient): Future[Enumerator[Array[Byte]]] = {
    val req = url(Endpoints.containerExport(id).toString).GET
    docker.dockerRequest(req).map {
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        Enumerator.fromStream(resp.getResponseBodyAsStream())
        
      case Left(t) => throw new DockerRequestException(s"export of container $id failed", docker, Some(t), Some(req))
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
      case Left(t) => throw new DockerRequestException(s"unable to restart container $id", docker, Some(t), Some(req))
    }
  }
  
  def containerKill(id: ContainerId)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerKill(id).toString).POST 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Left(t) => throw new DockerRequestException(s"unable to kill container $id", docker, Some(t), Some(req))
    }
  }
  
  def attachStream(id: ContainerId, stdin: Boolean = false, stdout: Boolean = true, stderr: Boolean = false, logs: Boolean = true)(implicit docker: DockerClient): Future[Enumerator[DockerRawStreamChunk]] = {
    val req = url(Endpoints.containerAttach(id, true, stdin, stdout, stderr, false).toString).POST
    docker.dockerRequest(req).map {
      case Right(resp) if resp.getStatusCode() == 400 => throw new DockerBadParameterException(s"unable to attach to container $id", docker, req)
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        val en = Enumerator.fromStream(resp.getResponseBodyAsStream())
        ???	// TODO: implement chunk parser
      case Left(t) => throw new DockerRequestException(s"attaching to stream on container $id failed", docker, Some(t), Some(req))
    }
  }
  
  def attach(id: ContainerId, stdin: Boolean = false, stdout: Boolean = true, stderr: Boolean = false, logs: Boolean = true)(implicit docker: DockerClient): Future[Enumerator[DockerRawStreamChunk]] = {
    val req = url(Endpoints.containerAttach(id, false, stdin, stdout, stderr, true).toString).POST
    docker.dockerRequest(req).map {
      case Right(resp) if resp.getStatusCode() == 400 => throw new DockerBadParameterException(s"unable to attach to container $id", docker, req)
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        val en = Enumerator.fromStream(resp.getResponseBodyAsStream())
        ???	// TODO: implement chunk parser
      case Left(t) => throw new DockerRequestException(s"attaching to container $id failed", docker, Some(t), Some(req))
    }
  }
  
  def containerWait(id: ContainerId)(action: Int => _)(implicit docker: DockerClient) = {
    val req = url(Endpoints.containerWait(id).toString).POST
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val statusCode: Option[Int] = json.flatMap(j => (j \ "StatusCode").asOpt[Int])
        statusCode.map(code => action(code))
    }
  }
  
  def containerRemove(id: ContainerId, withVolumes: Boolean = false)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.containerRemove(id, withVolumes).toString).DELETE 
    docker.dockerRequest(req).map { 
      case Right(resp) if (resp.getStatusCode() == 204) => true
      case Right(resp) if (resp.getStatusCode() == 400) => throw new DockerBadParameterException(s"removing container $id failed", docker, req)
      case Right(resp) if (resp.getStatusCode() == 404) => throw new NoSuchContainerException(id, docker)
      case Left(t) => throw new DockerRequestException(s"unable to kill container $id", docker, Some(t), Some(req))
    }
  }
  
  def containerCopyResource(id: ContainerId, resourcePath: String)(implicit docker: DockerClient): Future[Enumerator[Array[Byte]]] = {
    val json = Json.obj("Resource" -> resourcePath)
    val req = url(Endpoints.containerCopy(id).toString).POST << Json.prettyPrint(json) <:< Map("Content-Type" -> "application/json")
    docker.dockerRequest(req).map {
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        Enumerator.fromStream(resp.getResponseBodyAsStream())
      case Left(t) => throw new DockerRequestException(s"copying resource $resourcePath from container $id failed", docker, Some(t), Some(req))
    }
  }

  def containerCommit(id: ContainerId, repoTag: RepositoryTag, runConfig: Option[ContainerConfiguration] = None)(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[ContainerId] = {
    val req = url(Endpoints.containerCommit(id, repoTag.repo, repoTag.tag, runConfig.map(j => Json.prettyPrint(Json.toJson(j)))).toString).POST
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val newId: ContainerId = json.flatMap(j => (j \ "Id").asOpt[String]).map(ContainerId(_)).getOrElse(ContainerId.emptyId)
        newId
      case Left(t) => throw new DockerRequestException(s"commit container $id (tag: ${repoTag.toString}) failed", docker, Some(t), Some(req))
    }
  }
  
  def containerCommitWithMessage(id: ContainerId, repoTag: RepositoryTag, withMessage: (String, Option[String]), runConfig: Option[ContainerConfiguration] = None)(implicit docker: DockerClient, fmt: Format[ContainerConfiguration]): Future[ContainerId] = {
    val commitMsg = withMessage
    val req = url(Endpoints.containerCommit(id, repoTag.repo, repoTag.tag, runConfig.map(j => Json.prettyPrint(Json.toJson(j))), Some(commitMsg._1), commitMsg._2).toString).POST
    docker.dockerRequest(req).map { 
      case Right(resp) if resp.getStatusCode() == 404 => throw new NoSuchContainerException(id, docker)
      case Right(resp) if resp.getStatusCode() == 500 => throw new DockerInternalServerErrorException(docker)
      case Right(resp) if resp.getStatusCode() == 200 => 
        val json = Json.parse(resp.getResponseBody()).asOpt[JsObject]
        val newId: ContainerId = json.flatMap(j => (j \ "Id").asOpt[String]).map(ContainerId(_)).getOrElse(ContainerId.emptyId)
        newId
      case Left(t) => throw new DockerRequestException(s"commit container $id failed", docker, Some(t), Some(req))
    }
  }
  
}

trait DockerImagesApi {
  def images(all: Boolean = false)(implicit docker: DockerClient, fmt: Format[DockerImage]): Future[Seq[DockerImage]] = {
    val req = url(Endpoints.images(all).toString).GET
    docker.dockerJsonRequest[Seq[DockerImage]](req).map { 
      case Right(images) => images
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"list images request failed", docker, Some(t), Some(req))
    }
  }
  
  def imageCreate(repoTag: RepositoryTag, registry: Option[String] = None, fromSrc: Option[String] = None)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Seq[JsObject]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.imageCreate(repoTag.repo, fromSrc, Some(repoTag.repo), repoTag.tag, registry).toString).POST
    	case data => url(Endpoints.imageCreate(repoTag.repo, fromSrc, Some(repoTag.repo), repoTag.tag, registry).toString).POST <:< Map("X-Registry-Auth" -> data.asBase64Encoded)
    }
    
    docker.dockerRequestStream(req).flatMap { 
      case Right(en) => (en |>>> DockerIteratee.json).map(_.toSeq)
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"image pull ${repoTag.toString} (registry: $registry) request failed", docker, Some(t), Some(req))
    }
  }

  def imageInsertResource(image: String, imageTargetPath: String, sourceFileUrl: Uri)(implicit docker: DockerClient): Future[Seq[JsObject]] = {
    val req = url(Endpoints.imageInsert(image, imageTargetPath, java.net.URI.create(sourceFileUrl.toString)).toString).POST
    
    docker.dockerRequestStream(req).map { 
      case Right(en) => 
        // TODO: parse en to create tuples from json {"type":"body"} and {"status":"body", "progress":"progStr", "progressDetail":} 
        Seq.empty
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(t) => throw new DockerRequestException(s"insert resource $sourceFileUrl into image $image:$imageTargetPath request failed", docker, Some(t), Some(req))
    }
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
  
  def imagePush(image: String, registry: Option[String] = None)(implicit docker: DockerClient, auth: DockerAuth = DockerAnonymousAuth): Future[Seq[JsObject]] = {
    val req = auth match {
    	case DockerAnonymousAuth => url(Endpoints.imagePush(image, registry).toString).POST
    	case data => url(Endpoints.imagePush(image, registry).toString).POST <:< Map("X-Registry-Auth" -> data.asBase64Encoded)
    }
    
    docker.dockerRequestStream(req).map { 
      case Right(en) => 
        // TODO: parse en to create tuples from json {"type":"body"} -> (type, body)
        Seq.empty
      case Left(StatusCode(500)) => throw new DockerInternalServerErrorException(docker)
      case Left(StatusCode(404)) => throw new NoSuchImageException(image, docker)
      case Left(t) => throw new DockerRequestException(s"push image $image request failed", docker, Some(t), Some(req))
    }
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
  
  def imageRemove(image: String)(implicit docker: DockerClient): Future[Seq[JsObject]] = {
    val req = url(Endpoints.imageRemove(image).toString).DELETE
    
    docker.dockerJsonRequest[Seq[JsObject]](req).map { 
      case Right(output) => output
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
  
  def imageExport(image: String)(implicit docker: DockerClient): Future[(String, Enumerator[Array[Byte]])] = {
    val req = url(Endpoints.imageExport(image).toString).GET
    docker.dockerRequestStream(req).map {
      case Right(en) => 
        (s"$image.tar", en)
      case Left(StatusCode(500)) => throw new DockerRequestException(s"exporting image $image request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"exporting image $image request failed", docker, Some(t), Some(req))
    }
  }
  
  def imagesImport(tarFile: java.io.File)(implicit docker: DockerClient): Future[Boolean] = {
    val req = url(Endpoints.imagesLoad.toString).POST <<< tarFile
    docker.dockerRequest(req).map {
      case Right(resp) if (resp.getStatusCode() == 200) => true
      case Right(resp) if (resp.getStatusCode() == 500) => throw new DockerRequestException(s"importing image from $tarFile request failed", docker, None, Some(req))
      case Left(t) => throw new DockerRequestException(s"importing image from $tarFile request failed", docker, Some(t), Some(req))
    }
  }
}