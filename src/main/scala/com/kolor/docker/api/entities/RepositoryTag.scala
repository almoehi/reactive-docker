package com.kolor.docker.api.entities

import com.kolor.docker.api.InvalidRepositoryTagFormatException


class RepositoryTag private[RepositoryTag] (val repo: String, val tag: Option[String]) extends DockerEntity {
  override def toString = s"$repo/${tag.getOrElse("latest")}"
  override def equals(o: Any) = o match {
    case d:RepositoryTag => d.repo.eq(repo) && d.tag.eq(tag)
    case _ => false
  }
}

object RepositoryTag {
	  val pattern = """^([\w_\-0-9\./]+):?([\w_\-0-9\.:]*)$""".r
	  val patternNone = """^(<none>):?(<none>)*$""".r
    val tagPattern = """^([\w_\-0-9\.\:])*$""".r   // characters not allowed in tags

	  def apply(s: String): RepositoryTag = s match {
	    case pattern(repo, tag: String) => new RepositoryTag(repo, Some(tag).filter(_.nonEmpty))
	    case pattern(repo, _) => new RepositoryTag(repo, None)
	    case patternNone(_, _) => new RepositoryTag("none", Some("none"))	// there might be images with no tags (e.g. zombie images)
      case str if (com.netaporter.uri.Uri.parse(s"repo://$str").host.nonEmpty) =>
        // we have a URL like syntax where the host-part equals to the repo and the path-part equals to the tag
        val uri = com.netaporter.uri.Uri.parse(s"repo://$str")
        val tag: String = uri.pathParts.headOption.map(_.part).getOrElse("")

        val host = uri.port match {
          case Some(port) => s"${uri.host.get}:$port"
          case _ => uri.host.get
        }

        uri.pathParts.headOption.map(_.part).getOrElse("") match {
          case _ if (!tagPattern.findFirstIn(tag).isDefined) => throw InvalidRepositoryTagFormatException(s"$tag is an invalid repository tag", s)
          case pattern(repo, tag: String) => new RepositoryTag(s"$host/$repo", Some(tag).filter(_.nonEmpty))
          case pattern(repo, _) => new RepositoryTag(s"$host/$repo", None)
          case _ => throw InvalidRepositoryTagFormatException(s"$s is an invalid repository tag", s)
        }
		  case _ => throw InvalidRepositoryTagFormatException(s"$s is an invalid repository tag", s)
	  }
	  
	  def unapply(tag: RepositoryTag): Option[String] = {
	    Some(tag.toString)
	  }
	  
	  def create(repo: String, tag: Option[String] = None) = new RepositoryTag(repo, tag)
}