
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
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory
import com.netaporter.uri.Uri
import com.netaporter.uri.Uri

class DockerBuildSpec extends Specification {

  import com.kolor.docker.api.json.FormatsV112._
  
  implicit def defaultAwaitTimeout: Duration = Duration.create(40, SECONDS)
  
  implicit val docker = Docker("localhost", 2375)
  
  private val log = LoggerFactory.getLogger(getClass())
  
  /**
   * put your won credentials here otherwise some tests might fail
   */
  lazy val authInfo = DockerAuthCredentials("almoehi", "NHL2000", "almoehi@gmail.com", "https://index.docker.io/v1/")
  
  def await[T](f: Future[T]): T = {
    Await.result(f, defaultAwaitTimeout)
  }
  
  sequential
  
  "DockerApi" should {
    
   "be able to build from simple dockerfile given as String" in new DockerContext {
     val (it, en) = Concurrent.joined[Array[Byte]]
      val maybeRes = (en &> DockerEnumeratee.rawStream &> Enumeratee.map{el => 
        log.info(s"DockerBuildSpec: $el")
        el
      } |>>> Iteratee.head)
      
      await(docker.dockerBuildIterateeFrom("reactive-docker-build", true, false, true)(it){() =>
          Seq(
          """FROM dockerfile/java""",
          """RUN ["mkdir", "-p", "/opt/docker/logs"]""",
          """EXPOSE 9000 9443""",
          """WORKDIR /opt/docker""",
          """RUN ["chown", "-R", "root", "."]""",
          """USER root""",
          """ENTRYPOINT ["/bin/bash"]""",
          """CMD []"""
          )
        }.flatMap(_.run)
      )
      
      val res = await(maybeRes)
      res must beSome{raw:DockerRawStreamChunk => 
        raw.channel must be_>=(0)
      }
    }
    
  }
}