package com.kolor.docker.api.types

import com.kolor.docker.api._
import play.api.libs.json._

class ImageId private[ImageId] (val id: String) extends DockerEntity {
	override def toString: String = id
	override def equals(o: Any) = o match {
	  case s:String => id.equalsIgnoreCase(s)
	  case _ => false
	}
}
		
object ImageId {
	  val shortPattern = """^([a-z0-9A-Z])+$""".r
	  val longPattern = """^([a-z0-9A-Z])+$""".r

	  def apply(s: String): ImageId = s match {
	    case shortPattern(id) => new ImageId(s)
	    case longPattern(id) => new ImageId(s)
		case _ => throw InvalidImageIdFormatException(s"$s is an invalid image ID", s)
	  }
	  
	  def unapply(id: ImageId): Option[String] = {
	    Some(id.id)
	  }
	  
	  def emptyId = new ImageId("")
}