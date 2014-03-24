package com.kolor.docker.api.json

import play.api.libs.json._
import com.kolor.docker.api.types._
import play.api.libs.functional.syntax._
import org.joda.time.DateTime

class ISODateTimeString(string: String) {
  def isoDateTime: DateTime = try {
    org.joda.time.format.ISODateTimeFormat.dateTime().parseDateTime(string)
  } catch {
    case e: IllegalArgumentException => org.joda.time.format.ISODateTimeFormat.dateTimeNoMillis().parseDateTime(string)
    case t: Throwable => new org.joda.time.DateTime(0)
  }
}

trait PartialFormat[T <: DockerEntity] extends Format[T] {
  def partialReads: PartialFunction[JsValue, JsResult[T]]
  def partialWrites: PartialFunction[DockerEntity, JsValue]

  def writes(t: T): JsValue = partialWrites(t)
  def reads(json: JsValue) = partialReads.lift(json).getOrElse(JsError("unhandled json value"))
}

object Formats {

  implicit def String2ISODateTime(s: String) = new ISODateTimeString(s)

  implicit def int2Boolean(i: Int) = i match {
    case 0 => false
    case _ => true
  }

  implicit object ContainerIdFormat extends PartialFormat[ContainerId] {
    def partialReads: PartialFunction[JsValue, JsResult[ContainerId]] = {
      case o: JsString if (o.value.nonEmpty) => JsSuccess(ContainerId(o.as[String]))
      case _ => JsError("ContainerId is empty or invalid")
    }

    val partialWrites: PartialFunction[DockerEntity, JsValue] = {
      case oid: ContainerId => JsString(oid.id)
    }
  }

  implicit object ImageIdFormat extends PartialFormat[ImageId] {
    def partialReads: PartialFunction[JsValue, JsResult[ImageId]] = {
      case o: JsString if (o.value.nonEmpty) => JsSuccess(ImageId(o.as[String]))
      case _ => JsError("ImageId is empty or invalid")
    }

    val partialWrites: PartialFunction[DockerEntity, JsValue] = {
      case oid: ImageId => JsString(oid.id)
    }
  }

  implicit object RepositoryTagFormat extends PartialFormat[RepositoryTag] {
    def partialReads: PartialFunction[JsValue, JsResult[RepositoryTag]] = {
      case o: JsString if (o.value.nonEmpty) => JsSuccess(RepositoryTag(o.as[String]))
      case _ => JsError("repository tag is empty or invalid")
    }

    val partialWrites: PartialFunction[DockerEntity, JsValue] = {
      case tag: RepositoryTag => JsString(tag.toString)
    }
  }

  // Json writer to serialize DockerVolumes into string array 
  implicit val dockerVolumeFmt = Json.format[DockerVolume]

    
  val dateTimeToIsoWrite: Writes[org.joda.time.DateTime] = new Writes[org.joda.time.DateTime] {
    def writes(dt: org.joda.time.DateTime): JsValue = JsString(org.joda.time.format.ISODateTimeFormat.dateTime().print(dt))
  }

  val dateTimeToMillis: Writes[org.joda.time.DateTime] = new Writes[org.joda.time.DateTime] {
    def writes(dt: org.joda.time.DateTime): JsValue = JsNumber(dt.getMillis())
  }

  val containerInfoVolumesWrite: Writes[Seq[DockerVolume]] = new Writes[Seq[DockerVolume]] {
    def writes(seq: Seq[DockerVolume]): JsValue = {
      val m = seq.map(el => Map(el.containerPath -> el)).reduceLeft(_ ++ _)
      Json.toJson(m)
    }
  }

  val bindMountHostConfigWrite: Writes[Seq[DockerVolume]] = new Writes[Seq[DockerVolume]] {
    def writes(seq: Seq[DockerVolume]): JsValue = Json.arr(seq.map(_.toString))
  }

  val hostConfigPortBindingWrite: Writes[Map[String, DockerPortBinding]] = new Writes[Map[String, DockerPortBinding]] {
    def writes(ports: Map[String, DockerPortBinding]): JsValue = {
      val ret = Json.obj()
      ports.map {
        case (_, cfg) => Map(s"${cfg.privatePort}/${cfg.protocol.getOrElse("tcp")}" -> Json.arr(Json.obj("HostPort" -> cfg.publicPort, "HostIp" -> cfg.hostIp)))
      }

      ret
    }
  }

  val networkConfigPortBindingWrite: Writes[Seq[DockerPortBinding]] = new Writes[Seq[DockerPortBinding]] {
    def writes(ports: Seq[DockerPortBinding]): JsValue = {
      val ret = Json.obj()
      ports.map {
        case cfg => Map(s"${cfg.privatePort}/${cfg.protocol.getOrElse("tcp")}" -> Json.obj("HostPort" -> cfg.publicPort, "HostIp" -> cfg.hostIp))
      }

      ret
    }
  }

  val containerConfigPortBindingWrite: Writes[Map[String, DockerPortBinding]] = new Writes[Map[String, DockerPortBinding]] {
    def writes(ports: Map[String, DockerPortBinding]): JsValue = {
      val ret = Json.obj()
      ports.map {
        case (_, cfg) => Map(s"${cfg.privatePort}/${cfg.protocol.getOrElse("tcp")}" -> Json.obj("HostPort" -> cfg.publicPort, "HostIp" -> cfg.hostIp))
      }

      ret
    }
  }

  implicit val dockerErrorInfoFmt = Format(
    (
      (__ \ "code").readNullable[Int] and
      (__ \ "message").readNullable[String])(DockerErrorInfo.apply _),
    Json.writes[DockerErrorInfo])

  implicit val dockerProgressInfoFmt = Format(
    (
      (__ \ "current").read[Int] and
      (__ \ "total").read[Int] and
      (__ \ "start").readNullable[Long].map(_.map(new org.joda.time.DateTime(_))))(DockerProgressInfo.apply _),
    Json.writes[DockerProgressInfo])
    
    
  implicit val dockerStatusMessageFmt = Format(
    (
      ((__ \ "id").readNullable[String]) and
      (__ \ "stream").readNullable[String] and
      (__ \ "status").readNullable[String] and
      (__ \ "from").readNullable[String] and
      (__ \ "time").readNullable[Long].map(_.map(new org.joda.time.DateTime(_))) and
      ((__ \ "progressDetail").readNullable[DockerProgressInfo] orElse Reads.pure(None)) and // we need this dirty hack here, as progressDetail sometimes is an empty object {} which is not handleded properly by readNullable
      ((__ \ "errorDetail").read[DockerErrorInfo].map(e => Some(e)) or (__ \ "error").readNullable[DockerErrorInfo])
    )(DockerStatusMessage.apply _),
    Json.writes[DockerStatusMessage])
    
  implicit val dockerImageSearchResultFmt = Format(
    (
      (__ \ "name").read[String] and
      (__ \ "description").readNullable[String] and
      (__ \ "is_official").read[Boolean] and
      (__ \ "is_trusted").read[Boolean] and
      (__ \ "star_count").read[Int])(DockerImageSearchResult.apply _),
    Json.writes[DockerImageSearchResult])

  implicit val dockerImageHistoryInfoFmt = Format(
    (
      (__ \ "Id").read[ImageId](ImageIdFormat) and
      (__ \ "Created").read[Long].map(new org.joda.time.DateTime(_)) and
      (__ \ "CreatedBy").read[String] and
      (__ \ "tags").readNullable[Seq[String]] and
      (__ \ "Size").readNullable[Long])(DockerImageHistoryInfo.apply _),
    Json.writes[DockerImageHistoryInfo])


  implicit val containerChangelogFmt = Format(
    (
      (__ \ "Path").read[String] and
      (__ \ "Kind").read[Int])(ContainerChangelogRecord.apply _),
    Json.writes[ContainerChangelogRecord])

  implicit val portBindFmt = Format(
    (
      (__ \ "Port").read[Int] and
      (__ \ "HostPort").readNullable[Int] and
      (__ \ "HostIp").readNullable[String] and
      (__ \ "Type").readNullable[String])(DockerPortBinding.apply _),
    (
      (__ \ "Port").write[Int] and
      (__ \ "HostPort").writeNullable[Int] and
      (__ \ "HostIp").writeNullable[String] and
      (__ \ "Type").writeNullable[String])(unlift(DockerPortBinding.unapply)))

  implicit val dockerVersionFmt = Format(
    (
      (__ \ "Version").read[String] and
      (__ \ "GitCommit").readNullable[String] and
      (__ \ "GoVersio ").readNullable[String] and
      (__ \ "Arch").readNullable[String] and
      (__ \ "KernelVersion").readNullable[String] and
      (__ \ "Os").readNullable[String])(DockerVersion.apply _),
    Json.writes[DockerVersion])

  implicit val containerStateFmt = Format(
    (
      (__ \ "Running").read[Boolean] and
      (__ \ "Pid").read[Int] and
      (__ \ "ExitCode").read[Int] and
      (__ \ "StartedAt").readNullable[String].map { opt =>
        opt.map(_.isoDateTime)
      } and
      (__ \ "FinishedAt").readNullable[String].map { opt =>
        opt.map(_.isoDateTime)
      } and
      (__ \ "Ghost").read[Boolean])(ContainerState.apply _),
    (
      (__ \ "Running").write[Boolean] and
      (__ \ "Pid").write[Int] and
      (__ \ "ExitCode").write[Int] and
      (__ \ "StartedAt").writeNullable[org.joda.time.DateTime](dateTimeToIsoWrite) and
      (__ \ "FinishedAt").writeNullable[org.joda.time.DateTime](dateTimeToIsoWrite) and
      (__ \ "Ghost").write[Boolean])(unlift(ContainerState.unapply)))

  implicit val containerNetworkConfigFmt = Format(
    (
      (__ \ "IPAddress").readNullable[String] and
      (__ \ "IPPrefixLen").readNullable[Int] and
      (__ \ "Gateway").readNullable[String] and
      (__ \ "Bridge").readNullable[String] and
      (__ \ "PortMapping").readNullable[Seq[String]] and
      (__ \ "Ports").readNullable[Map[String, DockerPortBinding]].map(_.map(_.values.toSeq)))(ContainerNetworkConfiguration.apply _),
    (
      (__ \ "IPAddress").writeNullable[String] and
      (__ \ "IPPrefixLen").writeNullable[Int] and
      (__ \ "Gateway").writeNullable[String] and
      (__ \ "Bridge").writeNullable[String] and
      (__ \ "PortMapping").writeNullable[Seq[String]] and
      (__ \ "Ports").writeNullable[Seq[DockerPortBinding]](networkConfigPortBindingWrite))(unlift(ContainerNetworkConfiguration.unapply)))

  implicit val dockerAuthFmt = Format(
    (
      (__ \ "Username").read[String] and
      (__ \ "Password").read[String] and
      (__ \ "Email").read[String] and
      (__ \ "ServerAddress").read[String])(DockerAuth.apply _),
    (
      (__ \ "Username").write[String] and
      (__ \ "Password").write[String] and
      (__ \ "Email").write[String] and
      (__ \ "ServerAddress").write[String])(unlift(DockerAuth.unapply)))

  implicit val containerHostConfigFmt = Format(
    (
      (__ \ "Privileged").read[Boolean] and
      (__ \ "PublishAllPorts").read[Boolean] and
      (__ \ "Binds").readNullable[Seq[String]].map { opt =>
        opt.map(_.map { el =>
          DockerVolume.fromString(el)
        }.filter(_.isDefined).map(_.get))
      } and
      (__ \ "ContainerIdFile").readNullable[String] and
      (__ \ "LxcConf").readNullable[Map[String, String]] and
      (__ \ "PortBindings").readNullable[Map[String, JsObject]].map { opt =>
        val regex = """^(\d+)/(tcp|udp)$""".r
        opt.map(_.flatMap {
          case (regex(localPort, pType), cfg) => Map(s"$localPort/$pType" -> DockerPortBinding(localPort.toInt, (cfg \ ("HostIp")).asOpt[Int], Some(pType), (cfg \ ("HostPort")).asOpt[String]))
        })
      } and
      (__ \ "Links").readNullable[Seq[ContainerId]])(ContainerHostConfiguration.apply _),
    (
      (__ \ "Privileged").write[Boolean] and
      (__ \ "PublishAllPorts").write[Boolean] and
      (__ \ "Binds").writeNullable[Seq[DockerVolume]](bindMountHostConfigWrite) and
      (__ \ "ContainerIdFile").writeNullable[String] and
      (__ \ "LxcConf").writeNullable[Map[String, String]] and
      (__ \ "PortBindings").writeNullable[Map[String, DockerPortBinding]](hostConfigPortBindingWrite) and
      (__ \ "Links").writeNullable[Seq[ContainerId]])(unlift(ContainerHostConfiguration.unapply)))

  implicit val containerConfigFmt = Format(
    (
      (__ \ "Image").readNullable[String] and
      (__ \ "Cmd").readNullable[Seq[String]] and
      (__ \ "Hostname").readNullable[String] and
      (__ \ "User").readNullable[String] and
      (__ \ "Memory").readNullable[Long] and
      (__ \ "MemorySwap").readNullable[Long] and
      (__ \ "AttachStdin").readNullable[Boolean] and
      (__ \ "AttachStdout").readNullable[Boolean] and
      (__ \ "AttachStderr").readNullable[Boolean] and
      //(__ \ "PortSpecs").readNullable[Seq[String]] and
      (__ \ "Tty").readNullable[Boolean] and
      (__ \ "OpenStdin").readNullable[Boolean] and
      (__ \ "StdinOnce").readNullable[Boolean] and
      (__ \ "Env").readNullable[Seq[String]] and
      (__ \ "Dns").readNullable[String] and
      (__ \ "Volumes").readNullable[Map[String, DockerVolume]] and
      (__ \ "VolumesFrom").readNullable[ContainerId].orElse(Reads.pure(None)) and
      (__ \ "WorkingDir").readNullable[String] and
      (__ \ "ExposedPorts").readNullable[Map[String, JsObject]].map { opt =>
        val regex = """^(\d+)/(tcp|udp)$""".r
        opt.map(_.flatMap {
          case (regex(localPort, pType), cfg) => Map(s"$localPort/$pType" -> DockerPortBinding(localPort.toInt, (cfg \ ("HostIp")).asOpt[Int], Some(pType), (cfg \ ("HostPort")).asOpt[String]))
        })
      } and
      (__ \ "Entrypoint").readNullable[Seq[String]] and
      (__ \ "NetworkDisabled").readNullable[Boolean] and
      (__ \ "OnBuild").readNullable[Seq[String]])(ContainerConfiguration.apply _),
    (
      (__ \ "Image").writeNullable[String] and
      (__ \ "Cmd").writeNullable[Seq[String]] and
      (__ \ "Hostname").writeNullable[String] and
      (__ \ "User").writeNullable[String] and
      (__ \ "Memory").writeNullable[Long] and
      (__ \ "MemorySwap").writeNullable[Long] and
      (__ \ "AttachStdin").writeNullable[Boolean] and
      (__ \ "AttachStdout").writeNullable[Boolean] and
      (__ \ "AttachStderr").writeNullable[Boolean] and
      //(__ \ "PortSpecs").writeNullable[Seq[String]] and
      (__ \ "Tty").writeNullable[Boolean] and
      (__ \ "OpenStdin").writeNullable[Boolean] and
      (__ \ "StdinOnce").writeNullable[Boolean] and
      (__ \ "Env").writeNullable[Seq[String]] and
      (__ \ "Dns").writeNullable[String] and
      (__ \ "Volumes").writeNullable[Map[String, DockerVolume]] and
      (__ \ "VolumesFrom").writeNullable[ContainerId] and
      (__ \ "WorkingDir").writeNullable[String] and
      (__ \ "ExposedPorts").writeNullable[Map[String, DockerPortBinding]](containerConfigPortBindingWrite) and
      (__ \ "Entrypoint").writeNullable[Seq[String]] and
      (__ \ "NetworkDisabled").writeNullable[Boolean] and
      (__ \ "OnBuild").writeNullable[Seq[String]])(unlift(ContainerConfiguration.unapply)))

  implicit val containerInfoFmt = Format(
    (
      ((__ \ "ID").read[ContainerId] or (__ \ "id").read[ContainerId] or (__ \ "Id").read[ContainerId]) and
      (__ \ "Image").read[String] and
      (__ \ "Config").read[ContainerConfiguration] and
      (__ \ "State").read[ContainerState] and
      (__ \ "NetworkSettings").read[ContainerNetworkConfiguration] and
      (__ \ "HostConfig").read[ContainerHostConfiguration] and
      (__ \ "Created").read[String].map(_.isoDateTime) and
      (__ \ "Name").readNullable[String].map(o => o.map(_.stripPrefix("/"))) and
      (__ \ "Path").readNullable[String] and
      (__ \ "Args").readNullable[Seq[String]] and
      (__ \ "ResolvConfPath").readNullable[String] and
      (__ \ "HostnamePath").readNullable[String] and
      (__ \ "HostsPath").readNullable[String] and
      (__ \ "Driver").readNullable[String] and
      (__ \ "Volumes").readNullable[Map[String, DockerVolume]].map(_.map(_.values.toSeq)) and
      (__ \ "VolumesRW").readNullable[Map[String, Boolean]])(ContainerInfo.apply _),
    (
      (__ \ "ID").write[ContainerId] and
      (__ \ "Image").write[String] and
      (__ \ "Config").write[ContainerConfiguration] and
      (__ \ "State").write[ContainerState] and
      (__ \ "NetworkSettings").write[ContainerNetworkConfiguration] and
      (__ \ "HostConfig").write[ContainerHostConfiguration] and
      (__ \ "Created").write[DateTime](dateTimeToMillis) and
      (__ \ "Name").writeNullable[String] and
      (__ \ "Path").writeNullable[String] and
      (__ \ "Args").writeNullable[Seq[String]] and
      (__ \ "ResolvConfPath").writeNullable[String] and
      (__ \ "HostnamePath").writeNullable[String] and
      (__ \ "HostsPath").writeNullable[String] and
      (__ \ "Driver").writeNullable[String] and
      (__ \ "Volumes").writeNullable[Seq[DockerVolume]](containerInfoVolumesWrite) and
      (__ \ "VolumesRW").writeNullable[Map[String, Boolean]])(unlift(ContainerInfo.unapply)))

  implicit val containerFmt = Format(
    (
      (__ \ "Id").read[ContainerId] and
      (__ \ "Image").read[RepositoryTag] and
      (__ \ "Names").read[JsArray].map {
        case arr if (arr.value.size > 0) => Some(arr.value.seq.map {
          case JsString(s) => s.stripPrefix("/")
          case _ => ""
        }.filter(_.nonEmpty))
        case _ => None
      } and
      ((__ \ "Command").read[String] or (__ \ "Command").read[Boolean].map(_.toString)) and
      (__ \ "Created").read[Long].map(new org.joda.time.DateTime(_)) and
      (__ \ "Status").read[String] and
      (__ \ "Ports").read[JsArray].map { arr =>
        Seq.empty[String]
      } and
      (__ \ "SizeRw").readNullable[Long] and
      (__ \ "SizeRootFs").readNullable[Long])(Container.apply _),
    Json.writes[Container])

  implicit val imageFmt: Format[DockerImage] = Format(
    ((__ \ "Id").read[String] and
      (__ \ "ParentId").read[Option[String]] and
      (__ \ "RepoTags").readNullable[Seq[RepositoryTag]] and
      (__ \ "Created").read[Long].map(new org.joda.time.DateTime(_)) and
      (__ \ "Size").read[Long] and
      (__ \ "VirtualSize").read[Long])(DockerImage.apply _),
    Json.writes[DockerImage])

  implicit val imageInfoFmt: Format[DockerImageInfo] = Format(
    (
      (__ \ "id").read[ImageId](ImageIdFormat) and
      (__ \ "parent").readNullable[ImageId](ImageIdFormat) and
      (__ \ "created").read[String].map(_.isoDateTime) and
      (__ \ "container").readNullable[ContainerId](ContainerIdFormat) and
      (__ \ "container_config").readNullable[ContainerConfiguration] and
      (__ \ "docker_version").readNullable[String] and
      (__ \ "author").readNullable[String] and
      (__ \ "config").read[ContainerConfiguration] and
      (__ \ "architecture").readNullable[String] and
      (__ \ "Size").readNullable[Long])(DockerImageInfo.apply _),
    Json.writes[DockerImageInfo])

  implicit val dockerInfoFmt: Format[DockerInfo] = Format(
    ((JsPath \ "Containers").read[Int] and
      (JsPath \ "Debug").read[Int].map(int2Boolean(_)) and
      (JsPath \ "Driver").read[String] and
      (JsPath \ "DriverStatus").read[JsArray].map { arr =>
        val items = arr(0).asOpt[JsArray]
        val m = items.map(el => Map(el(0).asOpt[String].getOrElse("") -> el(1).asOpt[String].getOrElse("")))
        m.map(_.filterKeys(_.nonEmpty)).getOrElse(Map.empty)
      }.orElse(Reads.pure(Map.empty[String, String])) and
      (JsPath \ "ExecutionDriver").read[String] and
      (JsPath \ "IPv4Forwarding").read[Int].map(int2Boolean(_)) and
      (JsPath \ "Images").read[Int] and
      (JsPath \ "IndexServerAddress").read[String] and
      (JsPath \ "InitPath").read[String] and
      (JsPath \ "InitSha1").read[String] and
      (JsPath \ "KernelVersion").read[String] and
      (JsPath \ "MemoryLimit").read[Int].map(int2Boolean(_)) and
      (JsPath \ "NEventsListener").read[Int] and
      (JsPath \ "NFd").read[Int] and
      (JsPath \ "NGoroutines").read[Int] and
      (JsPath \ "SwapLimit").read[Int].map(int2Boolean(_)))(DockerInfo.apply _),
    Json.writes[DockerInfo])
}