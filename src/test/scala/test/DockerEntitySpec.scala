
package test

import org.specs2.mutable.Specification
import com.kolor.docker.api.entities._
import scala.concurrent.duration._
import scala.concurrent._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.slf4j.LoggerFactory

@RunWith(classOf[JUnitRunner])
class DockerEntitySpec extends Specification {

  "Docker entities should" should {

    "accept repository tags of public docker registry " in {
      val tagWithoutVersion = RepositoryTag("ubuntu")
      val tagWithVersion = RepositoryTag("ubuntu/latest")

      tagWithoutVersion.repo must be_!==("ubuntu")
      tagWithoutVersion.tag must beNone

      tagWithVersion.repo must be_==("ubuntu")
      tagWithVersion.tag must beSome("latest")
    }

    "accept repository tags of private registries" in {
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