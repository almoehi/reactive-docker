
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
import com.kolor.docker.api.json.Formats._
import play.api.libs.iteratee._
import org.slf4j.LoggerFactory
import com.netaporter.uri.Uri
import com.netaporter.uri.Uri

class DockerApiSpec extends Specification {

  implicit def defaultAwaitTimeout: Duration = Duration.create(40, SECONDS)
  
  implicit val docker = Docker("localhost", 2375)
  
  private val log = LoggerFactory.getLogger(getClass())
  
  /**
   * put your won credentials here otherwise some tests might fail
   */
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
    
    "list images" in image {
      val images = await(docker.images())
      images.size must be_>(0)
    }
    
    "create / pull a new image from busybox base image" in new DockerContext {
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
    
    "insert a resource into an image" in image {env:Image => 
      val res = await(docker.imageInsertResource("busybox", "/tmp/test.txt", java.net.URI.create("http://xchat.org/changelog.txt")))
      
      res must not be empty
    	res.size must be_>(0)
    	res(0) must beLike {
        	case Right(msg) => msg.status must not be empty
        	case Left(err) => err.message must not be empty
    	}
    }
    
    "inspect a docker image" in image {env:Image => 
      val info = await(docker.imageInspect(env.imageName))
      info.id.id must not beEmpty
    }
    
    "retrieve image history / changelog" in image {env:Image => 
      val hist = await(docker.imageHistory(env.imageName))
      hist.size must be_>(0)
    }
    
    "push image to a registry" in image {env:Image =>
      val res = await(docker.imagePush(env.imageName))
      
      res must not be empty
      res.size must be_>(0)
      res(0) must beLike {
        case Right(msg) => msg.status must not be empty
        case Left(err) => err.code must not be empty
      }
    }
    
    "tag an image" in image {env:Image =>
      try {
        val res = await(docker.imageTag(env.imageName, "dockerspec"))
        res must be_==(true)
      } finally {
        await(docker.imageRemove("dockerspec"))
      }
    }
    
    "remove an image" in new DockerContext {
      try {
        await(docker.imageCreate(RepositoryTag("busybox")))
        val res = await(docker.imageRemove("busybox"))
        res.size must be_>(0)
      } finally {
        try {
          await(docker.imageRemove("busybox"))
        } catch {
          case t:Throwable => // ignore
        }
      }
    }
    
    "search for images" in image {env:Image => 
      val res = await(docker.imageSearch(env.imageName))
      res.size must be_>(0)
    }
    
    "export an image to a tarball" in image{env:Image => 
      	val tmpFile = TempFile.create(s"docker-export-image")
      	val os = new java.io.FileOutputStream(tmpFile)
    	await(docker.imageExport("busybox")(Iteratee.foreach(b => os.write(b))).flatMap(_.run))
    	os.close()
    	log.info(s"saved exported image tarball to: ${tmpFile.getAbsolutePath()} Size=${tmpFile.length()}")
    	tmpFile.length().toInt must beGreaterThan(1024)
    }
    
    
    "import an image from a tarball" in image{env:Image => 
      	val tmpFile = TempFile.create(s"docker-export-image")
      	val os = new java.io.FileOutputStream(tmpFile)
    	await(docker.imageExport("busybox")(Iteratee.foreach(b => os.write(b))).flatMap(_.run))
    	os.close()
    	tmpFile.length().toInt must beGreaterThan(1024)
    	log.info(s"importing image tarball from: ${tmpFile.getAbsolutePath()} Size=${tmpFile.length()}")      	
      	val importRes = await(docker.imageImport(tmpFile))
      	importRes must be_==(true)
    }
    
    
    "list containers" in runningContainer {env:Container => 
      val containers = await(docker.containers(true))
      containers.size must be_>(0)
      val first = containers.head
      
      first.names.get.headOption must be like {
        case Some(s) => s must be_==(env.containerName)
      }
      
      first.image.repo must be_==(env.image.repo)
    }
    
    "inspect a simple container" in container {env:Container => 
      val info = await(docker.containerInspect(env.containerId))
      info.id.id must_!=("")
      info.id.id must_==(env.containerId.id)
    }
    
    "inspect a complex container" in complexContainer {env:Container =>
      val info = await(docker.containerInspect(env.containerId))
      info.id.id must be_==(env.containerId.id)
      todo
    }
    
    "list processes of a container" in runningContainer{env:Container => 
      val procs = await(docker.containerProcesses(env.containerId, Some("-a")))
      procs._2.size must be_>(0)
    }
    
    "retrieve container history / changelog" in container {env:Container => 
      val hist = await(docker.containerChangelog(env.containerId))
      hist.size must be_>=(0)
    }
    
    "export a container to a tarball" in container{env:Container => 
      	val tmpFile = TempFile.create(s"docker-export-container")
      	val os = new java.io.FileOutputStream(tmpFile)
    	await(docker.containerExportIteratee(env.containerId)(Iteratee.foreach(b => os.write(b))).flatMap(_.run))
    	os.close()    	
    	log.info(s"exported container ${env.containerId} to tarball: ${tmpFile.getAbsolutePath()} Size=${tmpFile.length()}")
    	tmpFile.length().toInt must beGreaterThan(1024)
    }
    
    "create a complex container" in image {env:Image =>
      val name = "reactive-docker-testContainer"
      val hostName = "test.host.com"
        
      val cfg = ContainerConfiguration(cmd = Some(Seq("date")), hostname = Some(hostName))
      val id = await(docker.containerCreate(env.imageName, cfg, Some(name)))._1
	      
      try {
	      val info = await(docker.containerInspect(id))
	      
	      info.id.id must be_==(id.id)
	      info.name must beLike {
	        case Some(s) => s must be_==(name)
	      }
	      
	      info.config.hostname must beLike {
	        case Some(s) => s must be_==(hostName)
	      }
      } finally {
	      await(docker.containerStop(id))
	      await(docker.containerRemove(id, true))
      }
    }
    
    "start a container" in container {env:Container =>
      val start = await(docker.containerStart(env.containerId))
      val info = await(docker.containerInspect(env.containerId))
      
      start must be_==(true)
      info.state.running must be_==(true)
    }
    
    "stop a container" in container {env:Container =>
      val startRes = await(docker.containerStart(env.containerId))
      val startInfo = await(docker.containerInspect(env.containerId))
      
      startRes must be_==(true)
      startInfo.state.running must be_==(true)
      
      val stopRes = await(docker.containerStop(env.containerId))
      val stopInfo = await(docker.containerInspect(env.containerId))

      stopRes must be_==(true)
      stopInfo.state.running must be_==(false)
    }
    
    "restart a container" in runningContainer {env:Container =>
      val startInfo = await(docker.containerInspect(env.containerId))

      startInfo.state.running must be_==(true)
      
      val res = await(docker.containerRestart(env.containerId))
      val info = await(docker.containerInspect(env.containerId))
      res must be_==(true)
      info.state.running must be_==(true)
    }
    
    "kill a container" in runningContainer {env:Container => 
      val res = await(docker.containerKill(env.containerId))
      val info = await(docker.containerInspect(env.containerId))
      
      res must be_==(true)
      info.state.running must be_==(false)

    }
    
    "wait for a container to terminate" in image {env:Image =>
      val cfg = ContainerConfig("busybox", Seq("/bin/sh", "-c", "sleep 15"))
      val containerId = await(docker.containerCreate("busybox", cfg))._1
      val run = docker.containerStart(containerId)
      
      val status = await(docker.containerWait(containerId){
        case status => 
          log.info(s"container terminated with Status=$status")
          status
      })
      docker.containerRemove(containerId)
      status must be_==(0)
    }
    
    "remove a container without its volumes" in container {env:Container => 
      val res = await(docker.containerRemove(env.containerId))
      res must be_==(true)
    }
    
    "remove a container with its volumes" in complexContainer {env:Container =>
      val stop = await(docker.containerStop(env.containerId))
      val res = await(docker.containerRemove(env.containerId, true))
      res must be_==(true)
    }
    
    "copy a resource from within a container" in runningContainer {env:Container =>
      val tmpFile = TempFile.create(s"docker-container-copy-resource")
      val os = new java.io.FileOutputStream(tmpFile)
      
      await(docker.containerCopyResourceIteratee(env.containerId, "/etc/hosts")(Iteratee.foreach(b => os.write(b))).flatMap(_.run))
      
      os.close()    	
      log.info(s"copied /etc/hosts of container ${env.containerId} to: ${tmpFile.getAbsolutePath()} Size=${tmpFile.length()}")
      tmpFile.length().toInt must beGreaterThan(1024)
    }
    
    "commit a container" in runningContainer {env:Container =>
      val cfg = ContainerConfiguration(cmd = Some(Seq("echo")))
      val info = await(docker.containerInspect(env.containerId))
      
      info.state.running must be_==(true)
      
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