
package test

import org.specs2.mutable.Specification
import com.kolor.docker.api._
import scala.concurrent.duration._
import scala.concurrent._
import com.kolor.docker.api.entities._
import org.slf4j.LoggerFactory

class DockerEntitySpec extends Specification with DefaultDockerAuth {


  implicit def defaultAwaitTimeout: Duration = Duration.create(40, SECONDS)
  
  implicit val docker = Docker()
  
  private val log = LoggerFactory.getLogger(getClass())
  
  def await[T](f: Future[T]): T = {
    Await.result(f, defaultAwaitTimeout)
  }
  
  sequential
  
  "DockerEntity" should {
    
    "RepositoryTag should accept several formats" in new DockerContext {
      val a = RepositoryTag("192.168.0.120:5000/judge2312121212")
      val b = RepositoryTag("192.168.0.120:5000/test:latest")
      val c = RepositoryTag("192.168.0.120:5000/test.latest")
      val d = RepositoryTag("192.168.0.120:5000/test-latest")

      a.repo must be_==("192.168.0.120:5000/judge2312121212")
      a.tag must beNone

      b.repo must be_==("192.168.0.120:5000/test")
      b.tag must beSome("latest")

      c.repo must be_==("192.168.0.120:5000/test.latest")
      c.tag must beNone

      d.repo must be_==("192.168.0.120:5000/test-latest")
      d.tag must beNone
    } 


    "RepositoryTag should not accept tags with spaces" in new DockerContext {
      {
        RepositoryTag("192.168.0.120:5000/test latest")
      } must throwA[InvalidRepositoryTagFormatException]
    }


    "RepositoryTag should accept repository tags of public docker registry " in {
      val tagWithoutVersion = RepositoryTag("ubuntu")
      val tagWithVersion = RepositoryTag("ubuntu:latest")

      tagWithoutVersion.repo must be_==("ubuntu")
      tagWithoutVersion.tag must beNone

      tagWithVersion.repo must be_==("ubuntu")
      tagWithVersion.tag must beSome("latest")
    }

    "RepositoryTag should accept repository tags of private registries" in {
      val tagWithPrivateRegistryWithVersion = RepositoryTag("192.168.0.120:5000/test:latest")
      tagWithPrivateRegistryWithVersion.repo must be_==("192.168.0.120:5000/test")
      tagWithPrivateRegistryWithVersion.tag must beSome("latest")

      val tagWithPrivateRegistryWithoutVersion = RepositoryTag("192.168.0.120:5000/test")
      tagWithPrivateRegistryWithoutVersion.repo must be_==("192.168.0.120:5000/test")
      tagWithPrivateRegistryWithoutVersion.tag must beNone
      val tagWithPrivateRegistryDomainWithVersion = RepositoryTag("some.tld.com/test:latest")
      tagWithPrivateRegistryDomainWithVersion.repo must be_==("some.tld.com/test")
      tagWithPrivateRegistryDomainWithVersion.tag must beSome("latest")
    }
  }
}