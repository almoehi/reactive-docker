import com.kolor.docker.api._
import com.kolor.docker.api.types._
import scala.concurrent.duration.Duration
import org.specs2.specification._
import java.util.concurrent.TimeUnit._
import scala.concurrent._
import org.specs2.Specification
import org.specs2.execute.AsResult
import play.api.libs.json._

package object test {
  
  class DockerContext extends Scope {
	  implicit lazy val docker: DockerClient = Docker("localhost")
	  implicit val timeout = Duration.create(60, SECONDS)
  }
  
  case class Image(imageCmd: Seq[String], imageTag: RepositoryTag) {
	  def imageName = imageTag.repo
	}
	
  case class Container(containerId: ContainerId, containerName: String, imageCmd: Seq[String])
	

	
  trait DockerEnv[T] extends AroundOutside[T] {
	  implicit val docker = Docker("localhost")
	  implicit val timeout = Duration.create(60, SECONDS)
  }
	
	
	/**
	 * provides a spec2 Around context with a basic busybox image
	 */
	def image:DockerEnv[Image] = new DockerEnv[Image] {
	  val env = new Image(Seq("/bin/sh", "date"), RepositoryTag.create("busybox", Some("latest")))
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      Await.result(docker.imageCreate(env.imageTag), timeout)
	      AsResult(t)
	    } finally {
	      Await.result(docker.imageRemove(env.imageName), timeout)
	    }
	  }
	  // prepare a valid ImageEnv
	  def outside: Image = env
	}
	
	/**
	 * provides a spec2 Around context with a created but nut running container
	 */
	def container:DockerEnv[Container] = new DockerEnv[Container] {
	  val env = {
	    val cmd = Seq("/bin/sh", "-c", "while true; do echo hello world; sleep 1; done")
	    val containerName = "reactive-docker"
	    val imageTag = RepositoryTag.create("busybox", Some("latest"))
	    val cfg = ContainerConfig("busybox", cmd)
	    Await.result(docker.imageCreate(imageTag), timeout)
	    implicit val fmt:Format[ContainerConfiguration] = com.kolor.docker.api.json.Formats.containerConfigFmt
		val containerId = Await.result(docker.containerCreate("busybox", cfg, Some(containerName)), timeout)._1	    
	    new Container(containerId, containerName, cmd)
	  }
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      AsResult(t)
	    } finally {
	      Await.result(docker.containerStop(env.containerId, 10), timeout)
	      Await.result(docker.containerRemove(env.containerId, true), timeout)
	      Await.result(docker.imageRemove("busybox"), timeout)
	    }
	  }
	  // prepare a valid Container env
	  def outside: Container = env
	}
	
	/**
	 * provides a spec2 Around context with a running container
	 */
	def runningContainer:DockerEnv[Container] = new DockerEnv[Container] {
	  val env = {
	    val cmd = Seq("/bin/sh", "-c", "while true; do echo hello world; sleep 1; done")
	    val containerName = "reactive-docker"
	    val imageTag = RepositoryTag.create("busybox", Some("latest"))
	    val cfg = ContainerConfig("busybox", cmd)
	    Await.result(docker.imageCreate(imageTag), timeout)
	    implicit val fmt:Format[ContainerConfiguration] = com.kolor.docker.api.json.Formats.containerConfigFmt
		val containerId = Await.result(docker.containerCreate("busybox", cfg, Some(containerName)), timeout)._1
	    
		implicit val hostFmt: Format[ContainerHostConfiguration] = com.kolor.docker.api.json.Formats.containerHostConfigFmt
		Await.result(docker.containerStart(containerId), timeout)
	    new Container(containerId, containerName, cmd)
	  }
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      AsResult(t)
	    } finally {
	      Await.result(docker.containerStop(env.containerId, 10), timeout)
	      Await.result(docker.containerRemove(env.containerId, true), timeout)
	      Await.result(docker.imageRemove("busybox"), timeout)
	    }
	  }
	  // prepare a valid Container env
	  def outside: Container = env
	}
	

	/**
	 * provides a spec2 Around context with a complex / full blown running container
	 */
	def complexContainer:DockerEnv[Container] = new DockerEnv[Container] {
	  val env = {
	    val cmd = Seq("/bin/sh", "-c", "while true; do echo hello world; sleep 1; done")
	    val containerName = "reactive-docker"
	    val imageTag = RepositoryTag.create("busybox", Some("latest"))
	    val cfg = ContainerConfig("busybox", cmd)
	    Await.result(docker.imageCreate(imageTag), timeout)
	    implicit val fmt:Format[ContainerConfiguration] = com.kolor.docker.api.json.Formats.containerConfigFmt
		val containerId = Await.result(docker.containerCreate("busybox", cfg, Some(containerName)), timeout)._1
	    
		implicit val hostFmt: Format[ContainerHostConfiguration] = com.kolor.docker.api.json.Formats.containerHostConfigFmt
		Await.result(docker.containerStart(containerId), timeout)
	    new Container(containerId, containerName, cmd)
	  }
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      AsResult(t)
	    } finally {
	      Await.result(docker.containerStop(env.containerId, 10), timeout)
	      Await.result(docker.containerRemove(env.containerId, true), timeout)
	      Await.result(docker.imageRemove("busybox"), timeout)
	    }
	  }
	  // prepare a valid Container env
	  def outside: Container = env
	}
}