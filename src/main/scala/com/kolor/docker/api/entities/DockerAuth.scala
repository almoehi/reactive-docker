package com.kolor.docker.api.entities

trait DockerAuth{
  private lazy val base64Encoder = new sun.misc.BASE64Encoder()

  def username: String
  def password: String
  def email: String
  def serverAddress: String
  def asBase64Encoded: String = base64Encoder.encode(s"""{"username":"$username", "password":"$password", "email":"$email", "serveraddress":"$serverAddress"}""".getBytes())
}

object DockerAuthCredentials {
  private val CREDENTIALS_FILE_PATH = "/credentials.properties"

  def fromResource(): DockerAuthCredentials = {
    val reader = new ConfigReader(scala.io.Source.fromURL(getClass.getResource(CREDENTIALS_FILE_PATH)))
    DockerAuthCredentials(
      reader.getProperty("username"),
      reader.getProperty("password"),
      reader.getProperty("email"),
      reader.getProperty("serverAddress")
    )
  }
}

trait DefaultDockerAuth extends DockerAuth {
  lazy val dockerCredentials = DockerAuthCredentials.fromResource()

  def username = dockerCredentials.username
  def password = dockerCredentials.password
  def email = dockerCredentials.email
  def serverAddress = dockerCredentials.serverAddress
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
