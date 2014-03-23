
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
import com.kolor.docker.api.types.DockerStatusMessage
import org.slf4j.LoggerFactory

@RunWith(classOf[JUnitRunner])
class DockerQuickSpec extends Specification {

  implicit def defaultAwaitTimeout: Duration = Duration(120, SECONDS)

  implicit val docker = Docker("localhost")

  val log = LoggerFactory.getLogger(getClass())

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

  "DockerApi should at least be able to" should {
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

    "pull an image" in new DockerContext {
      val res = await(docker.imageCreate(RepositoryTag("busybox")).flatMap(_ |>>> consumeStatusMessage))

      res must not be empty
      
      res.get must beLike {
        case Right(msg) => msg.status must not be empty
        case Left(err) => err.code must not be empty
      }

      val rm = await(docker.imageRemove("busybox"))
      rm.size must be_>(0)
    }

    "retrieve docker events" in new DockerContext {

      try {
        val maybeStream = docker.dockerEventsStream.flatMap(_ |>>> consumeStatusMessage)

        log.info("pulling busybox image")
        val maybePull = docker.imageCreate(RepositoryTag("busybox")).map { res =>
          log.info("image has been created")
          docker.imageRemove("busybox").map(_ => log.info("image has been removed"))
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
  }
}