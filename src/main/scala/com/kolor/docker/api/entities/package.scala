package com.kolor.docker.api

import org.joda.time.DateTime

package object entities {

  case class DockerErrorInfo(code: Option[Int] = None, message: Option[String] = None) extends DockerEntity {
    override def toString = s"DockerErrorInfo(code=${code}, message=${message.getOrElse("")})"
    def isEmpty = code.isEmpty && message.isEmpty
  }

  case class DockerProgressInfo(current: Int, total: Int, start: Option[DateTime] = None) extends DockerEntity

  case class DockerStatusMessage(
    id: Option[String] = None,
    stream: Option[String] = None,
    status: Option[String] = None,
    from: Option[String] = None,
    time: Option[DateTime] = None,
    progress: Option[DockerProgressInfo] = None,
    error: Option[DockerErrorInfo] = None) extends DockerEntity {

    override def toString = s"DockerStatusMessage(id=${id.getOrElse("none")}, progress=${progress.getOrElse("none")}, stream=${stream.getOrElse("")}, status=${status.getOrElse("")}, error=${error.map(_.toString).getOrElse("none")})"
    def isError = error.map(e => e.code.nonEmpty || e.message.nonEmpty).getOrElse(false)
  }

  case class Container(
    id: ContainerId,
    image: RepositoryTag,
    names: Option[Seq[String]],
    command: String,
    created: DateTime,
    status: String,
    ports: Seq[DockerPortBinding],
    sizeRw: Option[Long],
    sizeRootFs: Option[Long]) extends DockerEntity

  case class ContainerState(
    running: Boolean = false,
    pid: Int = 0,
    exitCode: Int = 0,
    startedAt: Option[org.joda.time.DateTime] = None,
    finishedAt: Option[org.joda.time.DateTime] = None,
    ghost: Boolean = false) extends DockerEntity

  case class ContainerNetworkConfiguration(
    ipAddress: Option[String],
    ipPrefixLen: Option[Int],
    gateway: Option[String],
    bridge: Option[String],
    portMapping: Option[Seq[String]] = None,
    ports: Option[Seq[DockerPortBinding]] = None) extends DockerEntity

  case class DockerPortBinding(privatePort: Int, publicPort: Option[Int] = None, protocol: Option[String] = None, hostIp: Option[String] = None) extends DockerEntity

  sealed trait ContainerNetworkingMode extends DockerEntity { def name: String }

  object ContainerNetworkingMode {
    case object Default extends ContainerNetworkingMode { val name = "bridge" }
    case object Bridge extends ContainerNetworkingMode { val name = "bridge" }
    case object Host extends ContainerNetworkingMode { val name = "host" }
    case object None extends ContainerNetworkingMode { val name = "none" }
    case class Container(name: String) extends ContainerNetworkingMode {
      def container = "" // TODO: extract container from string: container:<containerIdOrName>
    }
  }

  case class ContainerRestartPolicy(
    name: String,
    maximumRetryCount: Int) extends DockerEntity

  case class ContainerHostConfiguration(
    privileged: Boolean = false,
    publishAllPorts: Boolean = false,
    binds: Option[Seq[BindMountVolume]] = None,
    containerIdFile: Option[String] = None,
    lxcConf: Option[Map[String, String]] = None,
    networkMode: ContainerNetworkingMode = ContainerNetworkingMode.Default,
    restartPolicy: Option[ContainerRestartPolicy] = None,
    portBindings: Option[Map[String, DockerPortBinding]] = None,
    links: Option[Seq[String]] = None,
    capAdd: Seq[String] = Seq.empty, // new with 1.14
    capDrop: Seq[String] = Seq.empty // new with 1.14
    ) extends DockerEntity

  case class ContainerInfo(
    id: ContainerId,
    image: String,
    config: ContainerConfiguration,
    state: ContainerState,
    networkSettings: ContainerNetworkConfiguration,
    hostConfig: ContainerHostConfiguration,
    created: DateTime,
    name: Option[String] = None,
    path: Option[String] = None,
    args: Option[Seq[String]] = None,
    resolveConfPath: Option[String] = None,
    hostnamePath: Option[String] = None,
    hostsPath: Option[String] = None,
    driver: Option[String] = None,
    volumes: Option[Seq[DockerVolume]] = None,
    volumesRW: Option[Map[String, Boolean]] = None) extends DockerEntity

  case class ContainerChangelogRecord(
    path: String,
    kind: Int) extends DockerEntity

  case class DockerRawStreamChunk(channel: Int, size: Int, data: Array[Byte]) extends DockerEntity {
    def text = (new String(data, "utf-8")).trim
    override def toString = s"RawStreamChunk [$channel] '$text'"
  }

  case class DockerImage(
    id: String,
    parentId: Option[String],
    repoTags: Option[Seq[RepositoryTag]],
    created: DateTime,
    size: Long,
    virtualSize: Long) extends DockerEntity

  case class DockerImageInfo(
    id: ImageId,
    parent: Option[ImageId] = None,
    created: DateTime,
    container: Option[ContainerId],
    containerConfig: Option[ContainerConfiguration],
    dockerVersion: Option[String],
    author: Option[String],
    config: ContainerConfiguration,
    architecture: Option[String],
    size: Option[Long],
    comment: String) extends DockerEntity

  case class DockerImageHistoryInfo(
    id: ImageId,
    created: DateTime,
    createdBy: String,
    tags: Option[Seq[String]],
    size: Option[Long])

  case class DockerImageSearchResult(
    name: String,
    description: Option[String],
    isOfficial: Boolean = false,
    isTrusted: Boolean = false,
    starCount: Int = 0)

  case class DockerInfo(
    containers: Int,
    debug: Boolean,
    driver: String,
    driverStatus: Map[String, String],
    executionDriver: String,
    ipv4Forwarding: Boolean,
    images: Int,
    indexServerAddress: String,
    initPath: String,
    initSha1: String,
    kernelVersion: String,
    memoryLimit: Boolean,
    nEventsListener: Int,
    nFd: Int,
    nGoroutines: Int,
    swapLimit: Boolean,
    sockets: Seq[String] = Seq.empty // new with 1.13 - TODO: check exact format
    )

  case class DockerVersion(version: String, gitCommit: Option[String], goVersion: Option[String], arch: Option[String], kernelVersion: Option[String], os: Option[String], apiVersion: Option[String])

}