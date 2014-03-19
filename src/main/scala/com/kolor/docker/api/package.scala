package com.kolor.docker


package object api {
  import json.Formats
  import com.kolor.docker.api.types._
  
  sealed trait DockerAttachable { }
  
  case object Stdin extends DockerAttachable
  case object Stdout extends DockerAttachable
  case object Stderr extends DockerAttachable
}