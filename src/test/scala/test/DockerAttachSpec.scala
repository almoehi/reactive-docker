
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
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory

@RunWith(classOf[JUnitRunner])
class DockerAttachSpec extends Specification {

  implicit def defaultAwaitTimeout: Duration = Duration(20, SECONDS)

  implicit val docker = Docker("localhost")

  val log = LoggerFactory.getLogger(getClass())

  def await[T](f: Future[T]): T = {
    Await.result(f, defaultAwaitTimeout)
  }

  sequential

  "DockerApi" should {
    
    "be able to attach to stdout stream" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)
        
      docker.attachStream(env.containerId, false, true, false, false)(it).flatMap(_.run)
      
      val res = await(maybeRes)

      res must not be empty
      res.get.channel must be_==(0)
      res.get.text must be_==("hello world")
    }


    "be able to attach to stdout and retrieve data" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)

      await(docker.attach(env.containerId, false, true, false, false)(it).flatMap(_.run))

      val res = await(maybeRes)

      res must beEmpty  // attaching to stdout returns nothing so far
    }

    "be able to attach to logs stream" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)

      docker.attachStream(env.containerId, false, false, false, true)(it).flatMap(_.run)

      val res = await(maybeRes)

      log.info(s"response: $res")
      res must not be empty
    }

    "be able to attach to logs and retrieve data" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)

      await(docker.attach(env.containerId, false, false, false, true)(it).flatMap(_.run))

      val res = await(maybeRes)
      log.info(s"response: $res")
      res must beEmpty  // attaching to stdout returns nothing so far
    }
  }
}