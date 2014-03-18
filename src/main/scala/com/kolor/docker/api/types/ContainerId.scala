package com.kolor.docker.api.types

import com.kolor.docker.api._
import play.api.libs.json._

class ContainerId private[ContainerId] (val id: String) extends DockerEntity {
	override def toString: String = id
	override def equals(o: Any) = o match {
	  case s:String => id.equalsIgnoreCase(s)
	  case _ => false
	}
}
		
object ContainerId {
	  val shortPattern = """^([a-z0-9A-Z])+$""".r
	  val longPattern = """^([a-z0-9A-Z])+$""".r

	  def apply(s: String): ContainerId = s match {
	    case shortPattern(id) => new ContainerId(s)
	    case longPattern(id) => new ContainerId(s)
		case _ => throw InvalidContainerIdFormatException(s"$s is an invalid container ID", s)
	  }
	  
	  def unapply(id: ContainerId): Option[String] = {
	    Some(id.id)
	  }
	  
	  def emptyId = new ContainerId("")
}