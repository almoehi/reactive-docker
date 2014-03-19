
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
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import com.kolor.docker.api.json.Formats._


@RunWith(classOf[JUnitRunner])
class DockerQuickSpec extends Specification  {
  
  implicit def defaultAwaitTimeout: Duration = Duration(60, SECONDS)
  
  implicit val docker = Docker("localhost")
  
  def await[T](f: Future[T]): T = {
    Await.result(f, defaultAwaitTimeout)
  }
    
  sequential
    
  "DockerApi should at least be able to" should {
    
    "create, start, stop and remove a container" in container {env:Container => 
      env.containerId.id must not be empty
      val run = await(docker.containerStart(env.containerId))
      val info = await(docker.containerInspect(env.containerId))
      
      info.name must beSome(env.containerName)
      info.state.running must be_==(true)
    }
    
    "inspect a running container" in runningContainer {env:Container =>
      val info = await(docker.containerInspect(env.containerId))
      info.state.running must be_==(true)
    }
    
    "inspect an existing image" in image {env:Image =>
      val info = await(docker.imageInspect(env.imageName))
      info.id.id must not be empty
    }
    
    "pull an image" in new DockerContext {
      val res = await(docker.imageCreate(RepositoryTag("busybox")))
      res.size must be_>(0)
      
      res(0) must beLike {
        case Right(msg) => msg.status must not be empty
        case Left(err) => err.code must not be empty
      }
      
      val rm = await(docker.imageRemove("busybox"))
      rm.size must be_>(0)
    }
  }
}