
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
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.TimeUnit
import scala.util.Try

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

    "be able to attach to stdout + stdin stream and send commands to stdin" in ubuntu {env:Container =>

      await(docker.containerStart(env.containerId))
      log.info(s"container up and running: ${env.containerId}")

      // create stdout consumer which will collect the first output line
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)


      // generate stdin input
      val stdin = Enumerator.generateM[Array[Byte]]{
        // don't forget to append a \n to the shell command
        Future.successful(Some("uname -a\n".toCharArray.map(_.toByte)))
      } >>> Enumerator.eof

      // attach to stdout, connect stdin enumerator and run
      await(docker.attachStream(env.containerId, true, false, false, Some(stdin))(it).flatMap(_.run))

      // kill container, otherwise it would run forever
      docker.containerStop(env.containerId)

      // res should contain one DockerRawChunk with output of "uname -a" command
      val res = await(maybeRes)

      // log.info(s"result: $res")

      res must not be empty
      res.get.channel must be_==(0)
      res.get.text must startWith("Linux")
    }


    "be able to attach to stdout stream" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)
        
      docker.attachStream(env.containerId, true, false, false)(it).flatMap(_.run)
      
      val res = await(maybeRes)

      res must not be empty
      res.get.channel must be_==(0)
      res.get.text must be_==("hello world")
    }


    "be able to attach to stdout and retrieve data" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)

      await(docker.attach(env.containerId, true, false, false)(it).flatMap(_.run))

      val res = await(maybeRes)
      // TODO: returns no output with new container, how to test?
      res must beEmpty  // attaching to stdout returns nothing so far
    }

    "be able to attach to logs stream" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)

      docker.attachStream(env.containerId, false, false, true)(it).flatMap(_.run)

      val res = await(maybeRes)

      //log.info(s"response: $res")
      res must not be empty
    }

    "be able to attach to logs and retrieve data" in runningContainer {env:Container =>
      val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream |>>> Iteratee.head)

      await(docker.attach(env.containerId, false, false, true)(it).flatMap(_.run))

      val res = await(maybeRes)
      //log.info(s"response: $res")
      // TODO: returns no output with new container, how to test?
      res must beEmpty  // attaching to stdout returns nothing so far
    }

  }
}