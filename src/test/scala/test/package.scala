import com.kolor.docker.api._
import com.kolor.docker.api.types._
import scala.concurrent.duration.Duration
import org.specs2.specification._
import java.util.concurrent.TimeUnit._
import scala.concurrent._
import org.specs2.Specification
import org.specs2.execute.AsResult
import play.api.libs.json._
import org.slf4j.LoggerFactory
import play.api.libs.iteratee.Iteratee
import scala.concurrent.ExecutionContext.Implicits.global

package object test {
  
  private val log = LoggerFactory.getLogger(getClass())
  
  class DockerContext extends Scope {
	  implicit lazy val docker: DockerClient = Docker("localhost")
	  implicit val timeout = Duration.create(60, SECONDS)
  }
  
  case class Image(imageCmd: Seq[String], imageTag: RepositoryTag) {
	  def imageName = imageTag.repo
  }
	
  case class Container(containerId: ContainerId, containerName: String, image: RepositoryTag, imageCmd: Seq[String])
	
  trait DockerEnv[T] extends AroundOutside[T] {
	  implicit val docker = Docker("localhost")
	  implicit val timeout = Duration.create(60, SECONDS)
  }
	
	
	/**
	 * provides a spec2 Around context with a basic busybox image
	 */
	def image:DockerEnv[Image] = new DockerEnv[Image] {
	  val cmd = Seq("/bin/sh", "-c", "while true; do echo hello world; sleep 1; done")
	  val env = new Image(cmd, RepositoryTag.create("busybox", Some("latest")))
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      log.info(s"prepare image context - pulling busybox:latest ...")
	      Await.result(docker.imageCreateIteratee(env.imageTag)(Iteratee.ignore).flatMap(_.run), timeout)
	      AsResult(t)
	    } finally {
	      Await.result(docker.imageRemove(env.imageName), timeout)
	      log.info(s"shutdown & cleaned up image context")
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
	    log.info(s"prepare container context - pulling busybox:latest ...")

	    Await.result(docker.imageCreateIteratee(imageTag)(Iteratee.ignore).flatMap(_.run), timeout)
	    implicit val fmt:Format[ContainerConfiguration] = com.kolor.docker.api.json.Formats.containerConfigFmt
	    log.info(s"prepare container context - creating container $containerName (cmd: ${cmd.mkString})")

		val containerId = Await.result(docker.containerCreate("busybox", cfg, Some(containerName)), timeout)._1	
		log.info(s"prepare container context - container ready with  $containerId")
	    new Container(containerId, containerName, imageTag, cmd)
	  }
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      AsResult(t)
	    } finally {
	      try {
	    	  Await.result(docker.containerStop(env.containerId, 10), timeout)
		      Await.result(docker.containerRemove(env.containerId, true), timeout)
		      Await.result(docker.imageRemove("busybox"), timeout)
	      } catch {
	        case t:Throwable => // ignore
	      } finally {
	    	  log.info(s"shutdown & cleaned up container context")
	      }
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
	    log.info(s"prepare runningContainer context - pulling busybox:latest ...")

	    Await.result(docker.imageCreateIteratee(imageTag)(Iteratee.ignore).flatMap(_.run), timeout)
	    implicit val fmt:Format[ContainerConfiguration] = com.kolor.docker.api.json.Formats.containerConfigFmt
	    log.info(s"prepare runningContainer context - creating container $containerName (cmd: ${cmd.mkString})")

		val containerId = Await.result(docker.containerCreate("busybox", cfg, Some(containerName)), timeout)._1
		log.info(s"prepare runningContainer context - container ready with  $containerId, starting ...")

		implicit val hostFmt: Format[ContainerHostConfiguration] = com.kolor.docker.api.json.Formats.containerHostConfigFmt
		Await.result(docker.containerStart(containerId), timeout)
		log.info(s"prepare runningContainer context - container $containerId running")

	    new Container(containerId, containerName, imageTag, cmd)
	  }
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      AsResult(t)
	    } finally {
	      try {
	    	  Await.result(docker.containerStop(env.containerId, 10), timeout)
		      Await.result(docker.containerRemove(env.containerId, true), timeout)
		      Await.result(docker.imageRemove("busybox"), timeout)
	      } catch {
	        case t:Throwable => // ignore
	      } finally {
	    	  log.info(s"shutdown & cleaned up runningContainer context")
	      }
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
	    log.info(s"prepare runningComplexContainer context - pulling busybox:latest ...")

	    Await.result(docker.imageCreateIteratee(imageTag)(Iteratee.ignore).flatMap(_.run), timeout)
	    implicit val fmt:Format[ContainerConfiguration] = com.kolor.docker.api.json.Formats.containerConfigFmt
	    log.info(s"prepare runningComplexContainer context - creating container $containerName (cmd: ${cmd.mkString}) (cfg: ${cfg})")

		val containerId = Await.result(docker.containerCreate("busybox", cfg, Some(containerName)), timeout)._1
	    log.info(s"prepare runningComplexContainer context - creating container $containerName (cmd: ${cmd.mkString})")

		implicit val hostFmt: Format[ContainerHostConfiguration] = com.kolor.docker.api.json.Formats.containerHostConfigFmt
		Await.result(docker.containerStart(containerId), timeout)
		log.info(s"prepare runningComplexContainer context - container $containerId running")

	    new Container(containerId, containerName, imageTag, cmd)
	  }
	  
	  // create a context
	  def around[T : AsResult](t: =>T) = {
	    try {
	      AsResult(t)
	    } finally {
	      try {
	    	  Await.result(docker.containerStop(env.containerId, 10), timeout)
		      Await.result(docker.containerRemove(env.containerId, true), timeout)
		      Await.result(docker.imageRemove("busybox"), timeout)
	      } catch {
	        case t:Throwable => // ignore
	      } finally {
	    	  log.info(s"shutdown & cleaned up runningComplexContainer context")
	      }
	    }
	  }
	  // prepare a valid Container env
	  def outside: Container = env
	}
}