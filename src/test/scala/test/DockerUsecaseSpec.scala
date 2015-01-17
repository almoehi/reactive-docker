
package test

import org.specs2.mutable.Specification
import com.kolor.docker.api._
import scala.concurrent.duration._
import scala.concurrent._
import com.kolor.docker.api.entities._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory

@RunWith(classOf[JUnitRunner])
class DockerUsecaseSpec extends Specification {
  
  import com.kolor.docker.api.json.FormatsV112._

  implicit def defaultAwaitTimeout: Duration = Duration(20, SECONDS)

  implicit val docker = Docker()

  val log = LoggerFactory.getLogger(getClass())

  def await[T](f: Future[T]): T = {
    Await.result(f, defaultAwaitTimeout)
  }

  sequential

  "DockerApi should support the following use cases" should {

    "list processes of a container" in runningContainer{env:Container =>
      val procs = await(docker.containerProcesses(env.containerId, Some("-aux")))
      procs._2.size must be_>(0)
    }

    "Be able to attach before starting a container" in container { env: Container =>
      env.containerId.id must not be empty
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)

      val attached = docker.attachStream(env.containerId, true, true, true)(it).flatMap(_.run)
      val run = await(docker.containerStart(env.containerId))

      val res = await(maybeRes)
      res must not be empty
      res.get.channel must be_==(0)
      res.get.text must be_==("hello world")
    }
  }
}