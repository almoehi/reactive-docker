package com.kolor.docker


package object api {

  import com.kolor.docker.api.types._
  import com.kolor.docker.api.json.Formats._
  
  implicit val dockerJsonFormats = com.kolor.docker.api.json.Formats
  
  sealed trait DockerAttachable { }
    
  case object Stdin extends DockerAttachable
  case object Stdout extends DockerAttachable
  case object Stderr extends DockerAttachable
}