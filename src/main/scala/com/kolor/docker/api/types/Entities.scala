/**
 *
 */
package com.kolor.docker.api.types

import org.joda.time.DateTime
import play.api.libs.json._
import com.kolor.docker.api._


trait DockerEntity {
  
}


case class Container(
    id: ContainerId, 
    image: String, 
    names: Option[Seq[String]], 
    command: String, 
    created: DateTime, 
    status: String, 
    ports: Seq[String], 
    sizeRw: Option[Long], 
    sizeRootFs: Option[Long]
) extends DockerEntity

case class ContainerState(
    running: Boolean = false, 
    pid: Int = 0, 
    exitCode: Int = 0, 
    startedAt: Option[org.joda.time.DateTime] = None, 
    finishedAt: Option[org.joda.time.DateTime] = None, 
    ghost: Boolean = false
) extends DockerEntity

case class ContainerNetworkConfiguration(
	ipAddress: Option[String], 
    ipPrefixLen: Option[Int], 
    gateway: Option[String], 
    bridge: Option[String], 
    portMapping: Option[Seq[String]] = None,
    ports: Option[Seq[DockerPortBinding]] = None
) extends DockerEntity


case class DockerPortBinding(privatePort: Int, publicPort: Option[Int] = None, portType: Option[String] = None, hostIp: Option[String] = None) extends DockerEntity

case class ContainerHostConfiguration(
    privileged: Boolean = false, 
    publishAllPorts: Boolean = false,
    binds: Option[Seq[BindMountVolume]] = None, 
    containerIdFile: Option[String] = None, 
    lxcConf: Option[Map[String,String]] = None, 
    portBindings: Option[Map[String, DockerPortBinding]] = None, 
    links: Option[Seq[ContainerId]] = None
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
    volumesRW: Option[Map[String, Boolean]] = None
) extends DockerEntity
    
    
case class ContainerChangelogRecord(
    path: String,
    kind: Int
) extends DockerEntity

case class DockerRawStreamChunk(channel: Int, chunk: Array[Byte])
    
case class DockerImage(
    id: String,
    parentId: Option[String],
    repoTags: Option[Seq[RepositoryTag]],
    created: DateTime,
    size: Long,
    virtualSize: Long
) extends DockerEntity

case class DockerImageInfo(
    id: ImageId,
    parent: Option[ImageId],
    created: DateTime,
    container: Option[ContainerId],
    containerConfig: Option[ContainerConfiguration],
    dockerVersion: Option[String],
    author: Option[String],
    config: ContainerConfiguration,
    architecture: Option[String],
    size: Option[Long]
) extends DockerEntity


case class DockerImageHistoryInfo(
    id: ImageId,
    created: DateTime,
    createdBy: String
)

case class DockerImageSearchResult(
    name: String,
    description: Option[String],
    isOfficial: Boolean = false,
    isTrusted: Boolean = false,
    starCount: Int = 0
)

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
    swapLimit: Boolean
)

case class DockerVersion(version: String, gitCommit: Option[String], goVersion: Option[String], arch: Option[String], kernelVersion: Option[String], os: Option[String])

case class DockerEvent(status: String, id: ContainerId, from: RepositoryTag, time: DateTime)