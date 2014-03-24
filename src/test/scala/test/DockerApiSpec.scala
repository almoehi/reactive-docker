
package test

import org.specs2.mutable.Specification
import org.specs2.specification.AllExpectations
import com.kolor.docker.api._
import org.specs2.specification.Scope
import org.specs2.specification.Before
import scala.concurrent.duration.DurationConversions._
import scala.concurrent.duration._
import scala.concurrent._
import com.kolor.docker.api.types._
import org.joda.time.DateTime
import org.specs2.execute._
import org.specs2.specification._
import org.specs2.matcher.FutureMatchers.await
import scala.concurrent.ExecutionContext.Implicits.global
import com.kolor.docker.api.json.Formats._
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory

class DockerApiSpec extends Specification {

  implicit def defaultAwaitTimeout: Duration = Duration.create(40, SECONDS)
  
  implicit val docker = Docker("localhost")
  
  private val log = LoggerFactory.getLogger(getClass())
  
  lazy val authInfo = DockerAuthCredentials("user", "pass", "me@host.com", "https://index.docker.io/v1/")
  
  private val consumeStatusMessage = new Iteratee[Either[DockerErrorInfo, DockerStatusMessage], Option[Either[DockerErrorInfo, DockerStatusMessage]]] {
    def fold[B](folder: play.api.libs.iteratee.Step[Either[DockerErrorInfo, DockerStatusMessage], Option[Either[DockerErrorInfo, DockerStatusMessage]]] => Future[B])(implicit ec: ExecutionContext): Future[B] = {
      folder(play.api.libs.iteratee.Step.Cont {
        case Input.EOF => Done(None, Input.EOF)
        case Input.Empty => this
        case Input.El(e) => Done(Some(e), Input.EOF)
      })
    }
  }

  private def folder(step: play.api.libs.iteratee.Step[Either[DockerErrorInfo, DockerStatusMessage], Option[Either[DockerErrorInfo, DockerStatusMessage]]]): Future[Option[Either[DockerErrorInfo, DockerStatusMessage]]] = step match {
    case play.api.libs.iteratee.Step.Done(a, _) => future(a)
    case play.api.libs.iteratee.Step.Cont(k) => k(Input.EOF).fold({
      case play.api.libs.iteratee.Step.Done(a1, _) => Future.successful(a1)
      case _ => throw new Exception("Erroneous or diverging iteratee")
    })

    case s => throw new Exception("Erroneous iteratee: " + s)
  }
  
  def await[T](f: Future[T]): T = {
    Await.result(f, defaultAwaitTimeout)
  }
  
  sequential
  
  "DockerApi" should {
    "connect to a remote docker instance and retrieve its version" in new DockerContext {
      await(docker.dockerVersion).version must beMatching("""\d+\.\d+\.\d+""".r)
    } 
    
    "retrieve docker info" in new DockerContext {
      val info = await(docker.dockerInfo)
      info.containers must be_>=(0)
      info.memoryLimit must be_==(true)
      info.driverStatus.size must be_>=(0)
    }
    
    
    "retrieve docker events" in new DockerContext {
      try {
        val maybeStream = docker.dockerEventsStream.flatMap(_ |>>> consumeStatusMessage)

        log.info("pulling busybox image")
        
        val maybePull = docker.imageCreate(RepositoryTag("busybox")).flatMap { res =>
          (res |>>> Iteratee.ignore).flatMap{_ =>
              log.info("image has been created")
        	  docker.imageRemove("busybox").map(_ => log.info("image has been removed"))
          }
        }

        val events = await(maybeStream)

        events must not be empty
        events.get must beLike {
            case Left(err) => err.code must not be empty
            case Right(msg) => msg.id must not be empty
        }

      } finally {

        try {
          await(docker.imageRemove("busybox"))
        } catch {
          case t: Throwable => // ignore
        }

      }
    }
    
    "auth against docker" in new DockerContext {
      val auth = await(docker.dockerAuth(authInfo))
      auth must be_==(true)
    }
    
    "fail anonymous auth against docker" in new DockerContext {
      val auth = await(docker.dockerAuth(DockerAnonymousAuth))
      auth must be_==(false)
    }
    
    "build images from dockerfile" in new DockerContext {
      /*
      val build = await(docker.dockerBuild(new java.io.File(""), "testImage/latest", false, false))
      build.size must be_>(0)
      // TODO: should not contain any error objects
       */
      todo
    }
    
    "list images" in image {
      val images = await(docker.images())
      images.size must be_>(0)
    }
    
    "create / pull a new image from busybox base image" in new DockerContext {
      try {
    	val res = await(docker.imageCreate(RepositoryTag.create("busybox", Some("pullTest"))).flatMap(_ |>>> Iteratee.getChunks))
    	res must not be empty
    	res.size must be_>(0)
    	res(0) must beLike {
        	case Right(msg) => msg.status must not be empty
        	case Left(err) => err.message must not be empty
    	}
      } finally {
        docker.imageRemove("busybox")
      } 
    }
    
    "insert a resource into an image" in new DockerContext {
      todo
    }
    
    "inspect a docker image" in image {env:Image => 
      val info = await(docker.imageInspect(env.imageName))
      info.id.id must not beEmpty
    }
    
    "retrieve image history / changelog" in image {env:Image => 
      val hist = await(docker.imageHistory(env.imageName))
      hist.size must be_>(0)
    }
    
    "push image to a registry" in image {env:Image =>
      val res = await(docker.imagePush(env.imageName).flatMap(_ |>>> Iteratee.getChunks).recoverWith{
        case t:Throwable => 
          log.error(s"failed to push image", t)
          Future.successful(List.empty)
      })
      res must not be empty
      res.size must be_>(0)
      res(0) must beLike {
        case Right(msg) => msg.status must not be empty
        case Left(err) => err.code must not be empty
      }
    }
    
    "tag an image" in image {env:Image =>
      try {
        val res = await(docker.imageTag(env.imageName, "dockerspec"))
        res must be_==(true)
      } finally {
        await(docker.imageRemove("dockerspec"))
      }
    }
    
    "remove an image" in new DockerContext {
      try {
        val create = await(docker.imageCreate(RepositoryTag("busybox")).flatMap(_ |>>> Iteratee.ignore))
        val res = await(docker.imageRemove("busybox"))
        res.size must be_>(0)
      } finally {
        try {
          await(docker.imageRemove("busybox"))
        } catch {
          case t:Throwable => // ignore
        }
      }
    }
    
    "search for images" in image {env:Image => 
      val res = await(docker.imageSearch(env.imageName))
      res.size must be_>(0)
    }
    
    "export an image to a tarball" in todo
    
    "import an image from a tarball" in todo
    
    
    "list containers" in runningContainer {env:Container => 
      val containers = await(docker.containers(true))
      containers.size must be_>(0)
      val first = containers.head
      
      first.names.get.headOption must be like {
        case Some(s) => s must be_==(env.containerName)
      }
      
      first.image.repo must be_==(env.image.repo)
    }
    
    "inspect a simple container" in container {env:Container => 
      val info = await(docker.containerInspect(env.containerId))
      info.id.id must_!=("")
      info.id.id must_==(env.containerId.id)
    }
    
    "inspect a complex container" in complexContainer {env:Container =>
      val info = await(docker.containerInspect(env.containerId))
      info.id.id must be_==(env.containerId.id)
      todo
    }
    
    "list processes of a container" in runningContainer{env:Container => 
      val procs = await(docker.containerProcesses(env.containerId, Some("-a")))
      procs._2.size must be_>(0)
    }
    
    "retrieve container history / changelog" in container {env:Container => 
      val hist = await(docker.containerChangelog(env.containerId))
      hist.size must be_>=(0)
    }
    
    "export a container to a tarball" in todo
    
    "create a complex container" in image {env:Image =>
      val name = "reactive-docker-testContainer"
      val hostName = "test.host.com"
        
      val cfg = ContainerConfiguration(cmd = Some(Seq("date")), hostname = Some(hostName))
      val id = await(docker.containerCreate(env.imageName, cfg, Some(name)))._1
	      
      try {
	      val info = await(docker.containerInspect(id))
	      
	      info.id.id must be_==(id.id)
	      info.name must beLike {
	        case Some(s) => s must be_==(name)
	      }
	      
	      info.config.hostname must beLike {
	        case Some(s) => s must be_==(hostName)
	      }
      } finally {
	      await(docker.containerStop(id))
	      await(docker.containerRemove(id, true))
      }
    }
    
    "start a container" in new DockerContext {
      todo
    }
    
    "stop a container" in container {env:Container =>
      val res = await(docker.containerStart(env.containerId))
      res must be_==(true)
    }
    
    "restart a container" in new DockerContext {
      todo
    }
    
    "kill a container" in new DockerContext {
      todo
    }
    
    "attach to container stdin as stream" in new DockerContext {
      todo
    }
    
    "attach to container stderr (no streaming)" in new DockerContext {
      todo
    }
    
    "wait for a container to terminate" in new DockerContext {
      todo
    }
    
    "remove a container without its volumes" in new DockerContext {
      todo
    }
    
    "remove a container with its volumes" in new DockerContext {
      todo
    }
    
    "copy a resource from within a container" in new DockerContext {
      todo
    }
    
    "commit a container" in runningContainer {env:Container =>
      val cfg = ContainerConfiguration(cmd = Some(Seq("echo")))
      val info = await(docker.containerInspect(env.containerId))
      
      info.state.running must be_==(true)
      
      val id = await(docker.containerCommit(env.containerId, RepositoryTag.create("dockerspec", Some("committest")), Some(cfg)))
      id.id must not be empty
    }
    
    "commit a container with message and author" in runningContainer {env:Container =>
      val cfg = ContainerConfiguration(cmd = Some(Seq("echo")))
      val id = await(docker.containerCommitWithMessage(env.containerId, RepositoryTag.create("dockerspec", Some("commitmessagetest")), ("some commit message", Some("someAuthor")), Some(cfg)))
      id.id must not be empty
    }
  }
}