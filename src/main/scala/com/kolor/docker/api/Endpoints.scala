package com.kolor.docker.api

import com.netaporter.uri.dsl._
import com.netaporter.uri.Uri.parse
import com.netaporter.uri.Uri
import com.kolor.docker.api.types._
import com.kolor.docker.api.types.DockerAuth

object Endpoints {

  implicit protected def host(implicit docker: DockerClient) = docker.dockerHost
  implicit protected def port(implicit docker: DockerClient) = docker.dockerPort
  
  def baseUri(implicit docker: DockerClient): Uri = (s"http://$host:$port")
  
  def dockerInfo(implicit docker: DockerClient): Uri = {
    baseUri / "info"
  }
  
  def dockerPing(implicit docker: DockerClient): Uri = {
    baseUri / "_ping"
  }
  
  def dockerVersion(implicit docker: DockerClient): Uri = {
    baseUri / "version"
  }
  
  def dockerAuth(implicit docker: DockerClient): Uri = {
    baseUri / "auth"
  }
  
  def dockerBuild(tag: String, verbose: Boolean = false, noCache: Boolean = false, forceRm: Boolean = false)(implicit docker: DockerClient): Uri = {
    (baseUri / "build") ? ("q" -> verbose) & ("nocache" -> noCache) & ("t" -> tag) & ("forcerm" -> forceRm)
  }
  
  def dockerEvents(since: Option[Long] = None, until: Option[Long] = None)(implicit docker: DockerClient): Uri = {
    since match {
      case Some(l) if (l > 0) => until match {
        case Some(u) if (u > 0) => (baseUri / "events") ? ("since" -> l) & ("until" -> u)
        case _ => (baseUri / "events") ? ("since" -> l)
      }
      case _ => (baseUri / "events")
    }
  }
  
  /**
   * docker container endpoints
   */
  
  
  def containers(all:Boolean = true, limit: Option[Int] = None, sinceId: Option[String] = None, beforeId: Option[String] = None, showSize: Boolean = true)(implicit docker: DockerClient) = {
    (baseUri / "containers" / "json") ? ("all" -> all) & ("limit" -> limit) & ("sinceId" -> sinceId) & ("beforeId" -> beforeId) & ("showSize" -> showSize)
  }
  
  def containerCreate(name: Option[String] = None)(implicit docker: DockerClient): Uri = {
    (baseUri / "containers" / "create") ? ("name", name)
  }
  
  def containerInspect(id: ContainerId)(implicit docker: DockerClient): Uri = {
    baseUri / "containers" / id.toString / "json"
  }
  
  def containerProcesses(id: ContainerId, psArgs: Option[String] = None)(implicit docker: DockerClient): Uri = {
    (baseUri / "containers" / id.toString / "top") ? ("ps_args" -> psArgs)
  }
  
  def containerChangelog(id: ContainerId)(implicit docker: DockerClient): Uri = {
    baseUri / "containers" / id.toString / "changes"
  }
  
  def containerExport(id: ContainerId)(implicit docker: DockerClient): Uri = {
    baseUri / "containers" / id.toString / "export"
  }
  
  def containerStart(id: ContainerId)(implicit docker: DockerClient): Uri = {
    baseUri / "containers" / id.toString / "start"
  }
  
  def containerStop(id: ContainerId, timeoutToKill: Int = 60)(implicit docker: DockerClient): Uri = {
    (baseUri / "containers" / id.toString / "stop") ? ("t" -> timeoutToKill)
  }
  
  def containerRestart(id: ContainerId, timeoutToKill: Int = 60)(implicit docker: DockerClient): Uri = {
    (baseUri / "containers" / id.toString / "restart") ? ("t" -> timeoutToKill)
  }
  
  def containerKill(id: ContainerId)(implicit docker: DockerClient): Uri = {
    baseUri / "containers" / id.toString / "kill"
  }
  
  def containerAttach(id: ContainerId, stream: Boolean, stdin: Boolean = false, stdout: Boolean = true, stderr: Boolean = false, logs: Boolean = true)(implicit docker: DockerClient): Uri = {
    (baseUri / "containers" / id.toString / "attach") ? ("stream" -> stream) & ("stdin" -> stdin) & ("stdout" -> stdout) & ("stderr" -> stderr) & ("logs" -> logs)
  }
  
  def containerLogs(id: ContainerId, stream: Boolean, stdout: Boolean = true, stderr: Boolean = false, withTimestamps: Boolean = false)(implicit docker: DockerClient): Uri = {
    (baseUri / "containers" / id.toString / "logs") ? ("follow" -> stream) & ("stdout" -> stdout) & ("stderr" -> stderr) & ("timestamps" -> withTimestamps)
  }
  
  def containerWait(id: ContainerId)(implicit docker: DockerClient): Uri = {
    baseUri / "containers" / id.toString / "wait"
  }
  
  def containerRemove(id: ContainerId, withVolumes: Boolean = false, force: Boolean = false)(implicit docker: DockerClient): Uri = {
    (baseUri / "containers" / id.toString) ? ("v" -> withVolumes) & ("force" -> force)
  }
  
  def containerCopy(id: ContainerId)(implicit docker: DockerClient): Uri = {
    baseUri / "containers" / id.toString / "copy"
  }
  
  def containerCommit(id: ContainerId, repo: String, tag: Option[String], runConfig: Option[String] = None, message: Option[String] = None, author: Option[String] = None, pause: Boolean = true)(implicit docker: DockerClient): Uri = {
    val u = (baseUri / "commit")
    (u) ? ("container" -> id.toString) & ("repo" -> repo) & ("tag" -> tag) & ("m" -> message) & ("author" -> author) & ("pause" -> pause) // & ("run" -> runConfig)
  }
  
  /**
   * docker images endpoints
   */
  
  def images(all: Boolean = false)(implicit docker: DockerClient): Uri = {
    all match {
      case true => (baseUri / "images" / "json") ? ("all" -> all)
      case false => baseUri / "images" / "json" 
    }
  }
  
  def imageCreate(fromImage: String, fromSource: Option[String] = None, repo: Option[String], tag: Option[String], registry: Option[String])(implicit docker: DockerClient): Uri = {
    val u = (baseUri / "images" / "create")
    (u) ? ("fromImage" ->  fromImage) & ("fromSource" -> fromSource) & ("repo" -> repo) & ("tag" -> tag) & ("registry" -> registry)
  }
  
  def imageInsert(name: String, imageTargetPath: String, source: java.net.URI)(implicit docker: DockerClient): Uri = {    
    docker match {
      case d:DockerClientV19 => 
        val u = (baseUri / "images" / name / "insert")
        (u) ? ("path" -> imageTargetPath) & ("url" -> source.toString)
      case _ => throw new RuntimeException("imageInsert endpoint removed with api v1.12")
    }
  }
  
  def imageInspect(name: String)(implicit docker: DockerClient): Uri = {
    baseUri / "images" / name / "json"
  }
  
  def imageHistory(name: String)(implicit docker: DockerClient): Uri = {
    baseUri / "images" / name / "history"
  }
  
  def imagePush(name: String, registry: Option[String])(implicit docker: DockerClient): Uri = {
    (baseUri / "images" / name / "push") ? ("registry" -> registry)
  }
  
  def imageTag(name: String, repo: String, force: Boolean = false)(implicit docker: DockerClient): Uri = {
    (baseUri / "images" / name / "tag") ? ("repo" -> repo) & ("force" -> force)
  }
  
  def imageRemove(name: String, force: Boolean = false, noPrune: Boolean = false)(implicit docker: DockerClient): Uri = {
    (baseUri / "images" / name) ? ("force" -> force) & ("noprune" -> noPrune)
  }
  
  def imageSearch(term: String)(implicit docker: DockerClient): Uri = {
    (baseUri / "images" / "search") ? ("term" -> term)
  }
  
  def imageExport(image: String)(implicit docker: DockerClient): Uri = {
    baseUri / "images" / image / "get"
  }
  
  def imagesLoad(implicit docker: DockerClient): Uri = {
    baseUri / "images" / "load"
  }
  
}