package com.kolor.docker.api.entities

import play.api.libs.json._

sealed case class ContainerConfiguration(
    image: Option[String] = None,
    cmd: Option[Seq[String]] = None,
    hostname: Option[String] = None,
    user: Option[String] = None,
    memory: Option[Long] = None,
    memorySwap: Option[Long] = None,
    attachStdin: Option[Boolean] = None,
    attachStdout: Option[Boolean] = None,
    attachStderr: Option[Boolean] = None,
    //portSpecs: Option[Seq[String]] = None, // deprec
    tty: Option[Boolean] = None,
    openStdin: Option[Boolean] = None,
    stdinOnce: Option[Boolean] = None,
    env: Option[Seq[String]] = None,
    dns: Option[String] = None,
    volumes: Option[Map[String, DockerVolume]] = None,
    volumesFrom: Option[ContainerId] = None,
    workingDir: Option[String] = None,
    exposedPorts: Option[Map[String, DockerPortBinding]] = None,
    entryPoint: Option[Seq[String]] = None,
    networkDisabled: Option[Boolean] = Some(false),
    onBuild: Option[Seq[String]] = None
) extends DockerEntity

object ContainerConfig {
  
  def apply(json: JsObject)(implicit fmt: Format[ContainerConfiguration]): ContainerConfiguration = {
    val res = Json.fromJson[ContainerConfiguration](json)(fmt)
    res.asOpt match {
      case Some(c) => c
      case _ => throw new RuntimeException(s"failed to serialize container config to json: " + Json.prettyPrint(json))
    }
  }
  
  def apply(json: String)(implicit fmt: Format[ContainerConfiguration]): ContainerConfiguration = {
    val res = Json.fromJson[ContainerConfiguration](Json.parse(json))(fmt)
    res.asOpt match {
      case Some(c) => c
      case _ => throw new RuntimeException(s"failed to serialize container config to json: " + json)
    }
  }
  
  def apply(image: String, cmd: Seq[String]): ContainerConfiguration = ContainerConfiguration(Some(image), Some(cmd))
}