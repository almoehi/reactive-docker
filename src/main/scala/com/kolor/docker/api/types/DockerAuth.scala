package com.kolor.docker.api.types

import play.api.libs.json._

trait DockerAuth{
  def username: String
  def password: String
  def email: String
  def serverAddress: String
  def asBase64Encoded: String = ""
}

case class DockerAuthCredentials(
    username: String,
    password: String,
    email: String,
    serverAddress: String
) extends DockerAuth


case object DockerAnonymousAuth extends DockerAuth {
  val username = "anonymous"
  val password = "anonymous"
  val email = "anonymous@localhost"
  val serverAddress = "https://index.docker.io/v1/"
}

object DockerAuth {
  def apply(user: String, password: String, email: String, server: String): DockerAuth = DockerAuthCredentials(user, password, email, server)

  def unapply(auth: DockerAuth): Option[(String, String, String,String)] = auth match {
    case auth => Some((auth.username, auth.password, auth.email, auth.serverAddress))
  }
}
