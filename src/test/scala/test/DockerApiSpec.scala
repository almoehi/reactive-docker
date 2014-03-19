
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
import play.api.libs.iteratee.Enumeratee
import play.api.libs.iteratee.Iteratee

class DockerApiSpec extends Specification {

  implicit def defaultAwaitTimeout: Duration = Duration.create(60, SECONDS)
  
  implicit val docker = Docker("localhost")
  
  lazy val authInfo = DockerAuthCredentials("user", "pass", "me@host.com", "https://index.docker.io/v1/")
  
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
      val events = await(docker.dockerEvents())
      val take1AndConsume = Enumeratee.take[Seq[Either[DockerErrorInfo, DockerStatusMessage]]](1) &>> Iteratee.consume()
      val res = await(events |>>> take1AndConsume)
      res.size must be_>=(0)
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
    
    "list images" in new DockerContext {
      val images = await(docker.images())
      images.size must be_>(0)
    }
    
    "create a new image from busybox base image" in new DockerContext {
      val res = await(docker.imageCreate(RepositoryTag("busybox")))
      res.size must be_>(0)
    }
    
    "insert a resource into an image" in new DockerContext {
      todo
    }
    
    "inspect a docker image" in new DockerContext {
      val info = await(docker.imageInspect("busybox"))
      info.id.id must not beEmpty
    }
    
    "retrive image history / changelog" in new DockerContext {
      val hist = await(docker.imageHistory("busybox"))
      hist.size must be_>(0)
    }
    
    "push image to a registry" in new DockerContext {
      val res = await(docker.imagePush("dockerspec"))
      res.size must be_>(0)
    }
    
    "tag an image" in new DockerContext {
      val res = await(docker.imageTag("dockerspec", "dockerspec"))
      res must be_==(true)
    }
    
    "remove an image" in new DockerContext {
      val create = await(docker.imageCreate(RepositoryTag("busybox")))
      val res = await(docker.imageRemove("busybox"))
      res must be_==(true)
    }
    
    "search for images" in new DockerContext {
      val res = await(docker.imageSearch("busybox"))
      res.size must be_>(0)
    }
    
    "export an image to a tarball" in todo
    
    "import an image from a tarball" in todo
    
    
    "list containers" in new DockerContext {
      val containers = await(docker.containers(true))
      containers.size must be_>(0)
      val first = containers.head
      
      first.names.get.headOption must be like {
        case Some(s) => s must startWith("reactive-docker-simple") 
      }
      
      first.image must be_==("busybox:latest")
    }
    
    "inspect a simple container" in container {env:Container => 
      val info = await(docker.containerInspect(env.containerId))
      info.id.id must_!=("")
      info.id.id must_==(env.containerId.id)
    }
    
    "inspect a complex container" in complexContainer {env:Container =>
      val info = await(docker.containerInspect(env.containerId))
      info.id.id must be_==(env.containerId.id)
    }
    
    "list processes of a container" in todo
    
    "retrieve container history / changelog" in container {env:Container => 
      val hist = await(docker.containerChangelog(env.containerId))
      hist.size must be_>=(0)
    }
    
    "export a container to a tarball" in todo
    
    "create a complex container" in new DockerContext {
      val name = "reactive-docker-testContainer"
      val hostName = "test.host.com"
        
      val cfg = ContainerConfiguration(cmd = Some(Seq("date")), hostname = Some(hostName))
      
      val id = await(docker.containerCreate("busybox", cfg, Some(name)))._1
      
      val info = await(docker.containerInspect(id))
      
      info.id.id must be_==(id.id)
      info.name must beLike {
        case Some(s) => s must be_==(name)
      }
      
      info.config.hostname must beLike {
        case Some(s) => s must be_==(hostName)
      }
      
      await(docker.containerStop(id))
      await(docker.containerRemove(id, true))
    }
    
    "start a container" in new DockerContext {
      todo
    }
    
    "stop a container" in new DockerContext {
      todo
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