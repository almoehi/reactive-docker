package com.kolor.docker.api.types

import com.kolor.docker.api.InvalidRepositoryTagFormatException

class RepositoryTag private[RepositoryTag] (val repo: String, val tag: Option[String]) extends DockerEntity {
  override def toString = s"$repo/${tag.getOrElse("latest")}"
  override def equals(o: Any) = o match {
    case tag:RepositoryTag => tag.repo.eq(repo) && tag.tag.eq(tag)
    case _ => false
  }
}

object RepositoryTag {
	  val pattern = """^(\w+):?(\w*)$""".r
	  val patternNone = """^(<none>):?(<none>)*$""".r

	  def apply(s: String): RepositoryTag = s match {
	    case pattern(repo, tag: String) => new RepositoryTag(repo, Some(tag))
	    case pattern(repo, _) => new RepositoryTag(repo, None)
	    case patternNone(_, _) => new RepositoryTag("none", Some("none"))	// there might be images with no tags (e.g. zombie images)
		case _ => throw InvalidRepositoryTagFormatException(s"$s is an invalid repository tag", s)
	  }
	  
	  def unapply(tag: RepositoryTag): Option[String] = {
	    Some(tag.toString)
	  }
	  
	  def create(repo: String, tag: Option[String]) = new RepositoryTag(repo, tag)
}