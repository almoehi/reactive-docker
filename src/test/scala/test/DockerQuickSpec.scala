
package test

import org.specs2.mutable.Specification
import org.specs2.specification.AllExpectations
import com.kolor.docker.api._
import org.specs2.specification.Scope
import org.specs2.specification.Before
import scala.concurrent.duration.DurationConversions._
import scala.concurrent.duration._
import scala.concurrent._
import com.kolor.docker.api.entities._
import org.joda.time.DateTime
import org.specs2.execute._
import org.specs2.specification._
import org.specs2.matcher.FutureMatchers.await
import scala.concurrent.ExecutionContext.Implicits.global
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory

@RunWith(classOf[JUnitRunner])
class DockerQuickSpec extends Specification with DefaultDockerAuth {
  
  import com.kolor.docker.api.json.FormatsV112._

  implicit def defaultAwaitTimeout: Duration = Duration(20, SECONDS)

  implicit val docker = Docker()

  val log = LoggerFactory.getLogger(getClass())

  def await[T](f: Future[T]): T = {
    Await.result(f, defaultAwaitTimeout)
  }

  sequential

  "DockerApi should at least be able to" should {

    "ping docker host" in new DockerContext {
      val res = await(docker.dockerPing())
      res must be_==(true)
    }
    
    "list images" in image {
      val images = await(docker.images())
      images.size must be_>(0)
    }
    
    "create, start, stop and remove a container" in container { env: Container =>
      env.containerId.id must not be empty
      val run = await(docker.containerStart(env.containerId))
      val info = await(docker.containerInspect(env.containerId))

      info.name must beSome(env.containerName)
      info.state.running must be_==(true)
    }

    "inspect a running container" in runningContainer { env: Container =>
      val info = await(docker.containerInspect(env.containerId))
      info.state.running must be_==(true)
    }

    "inspect an existing image" in image { env: Image =>
      val info = await(docker.imageInspect(env.imageName))
      info.id.id must not be empty
    }
    
    
    "create / pull a new image from busybox base image" in new DockerContext {
      
      try {
        await(docker.imageRemove("busybox"))
      } catch {
        case e:NoSuchImageException => // ignore
      }
      
      try {
      	val res = await(docker.imageCreate(RepositoryTag.create("busybox", Some("pullTest"))))
      	
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
    
    "pull an image" in new DockerContext { 
      
      try {
        await(docker.imageRemove("busybox"))
      } catch {
        case e:NoSuchImageException => // ignore
      }
      
      val res = await(docker.imageCreate(RepositoryTag("busybox")))
      
      res must not be empty
      res.size must be_>(0)
      
      res(0) must beLike {
        case Right(msg) => msg.status must not be empty
        case Left(err) => err.code must not be empty
      }

      val rm = await(docker.imageRemove("busybox"))
      rm.size must be_>(0)
    }

    "retrieve docker events" in new DockerContext {
      try {        
        val (it, en) = Concurrent.joined[Array[Byte]]
        val headOption = (en &> DockerEnumeratee.statusStream() |>>> Iteratee.head)

        // this an infinite stream, so the connection won't terminate until the given iteratee is done
    	docker.dockerEventsStreamIteratee(it).map {i =>
    		log.info(s"connection to /events endpoint redeemed")
    		i.run
        }
        
        log.info("pulling busybox image")
        (docker.imageCreate(RepositoryTag("busybox"))).map{u => 
        	log.info("image has been created")
        	docker.imageRemove("busybox").map{_ =>
        		log.info(s"image has been removed")
        	}.recover{
        		case t:Throwable => log.error(s"imageRemove error", t)
        	}
        }
		
        val event = Await.result(headOption, Duration.Inf)

        event must not be empty
        event.get must beLike {
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

   "push image to a registry" in image {env:Image =>
  	  lazy val authInfo = dockerCredentials //DockerAuthCredentials("user", "pass", "me@host.com", "https://index.docker.io/v1/")

  	  val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.statusStream() |>>> Iteratee.getChunks)
        
      await(docker.imagePush(env.imageName))
      
      val res = await(maybeRes)
      res must not be empty
      res.size must be_>(0)
      res(0) must beLike {
        case Right(msg) => msg.status must not be empty
        case Left(err) => err.code must not be empty
      }
    }
    
    "copy a resource from within a container" in runningContainer {env:Container =>
      val tmpFile = TempFile.create(s"docker-container-copy-resource")
      val os = new java.io.FileOutputStream(tmpFile)

      val it = docker.containerCopyResourceIteratee(env.containerId, "/etc/hosts")(Iteratee.foreach(b => os.write(b))).flatMap(_.run)

      val res = await(it)
      os.close()    	
      log.info(s"copied /etc/hosts of container ${env.containerId} to: ${tmpFile.getAbsolutePath()} Size=${tmpFile.length()}")
      tmpFile.length().toInt must beGreaterThan(1024)
    }
  }
}