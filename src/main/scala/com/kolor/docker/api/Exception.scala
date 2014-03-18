package com.kolor.docker.api

import com.kolor.docker.api.types.ContainerId



trait DockerException extends Exception {
  def message: String
  override def getMessage():String = message
  override def getLocalizedMessage():String = message
}


trait DockerApiException extends DockerException {
  def client: DockerClient
}

case class DockerJsonParseError(message: String, json: String, cause: Option[Throwable] = None) extends DockerException {
	cause.map(initCause(_))
    def this(message: String) = this(message, null)
}

case class InvalidContainerIdFormatException(message: String, id: String, cause: Option[Throwable] = None) extends DockerException {
	cause.map(initCause(_))
    def this(message: String) = this(message, null)
}

case class InvalidImageIdFormatException(message: String, id: String, cause: Option[Throwable] = None) extends DockerException {
	cause.map(initCause(_))
    def this(message: String) = this(message, null)
}

case class InvalidRepositoryTagFormatException(message: String, tag: String, cause: Option[Throwable] = None) extends DockerException {
	cause.map(initCause(_))
    def this(message: String) = this(message, null)
}

case class DockerOperationException(message: String, cause: Option[Throwable] = None) extends DockerException {
	cause.map(initCause(_))
    def this(message: String) = this(message, null)
}

case class DockerRequestException(message: String, client: DockerClient, cause: Option[Throwable] = None, request: Option[dispatch.Req]) extends DockerApiException {
	cause.map(initCause(_))
}
case class DockerResponseParseError(message: String, client: DockerClient, response: String, cause: Option[Throwable] = None) extends DockerApiException {
	cause.map(initCause(_))
}

case class DockerResponseException(message: String, client: DockerClient, responseCode: Int, cause: Option[Throwable] = None) extends DockerApiException {
	cause.map(initCause(_))
}

case class DockerInternalServerErrorException(client: DockerClient, message: String="internal server error") extends DockerApiException

case class DockerBadParameterException(message: String, client: DockerClient, request: dispatch.Req, cause: Option[Throwable] = None) extends DockerApiException {
	cause.map(initCause(_))
}

case class ContainerNotRunningException(id: ContainerId, client: DockerClient) extends DockerApiException {
  def message = s"container $id is not running"
}

case class NoSuchImageException(image: String, client: DockerClient) extends DockerApiException {
  def message = s"image $image doesn't exist"
}

case class DockerConflictException(message: String, client: DockerClient) extends DockerApiException


case class NoSuchContainerException(id: ContainerId, client: DockerClient) extends DockerApiException {
  def message = s"container $id doesn't exist"
}