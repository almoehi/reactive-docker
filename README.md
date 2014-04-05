reactive-docker
===============

A fully reactive client for the docker remote API.


* fully async and non-blocking design
* async streaming capabilities using enumerators (e.g. attaching to docker events stream)
* supports latest docker remote API v1.10

reactive-docker currently supports the following operations:

* list images and containers
* create/pull and remove images
* create containers from images
* start, stop and commit containers
* inspect containers and images
* retrieve history / changelog of images
* retrieve list of running processes of containers
* attach to docker events stream
* attach to stdout, stderr and logs (both streaming &)

Installation
=============
As reactive-docker still is in a very early stage of development no artifacts have been published yet. To use it you simply need to:

* git clone
* ```sbt publishLocal```
* include into your SBT project ``` "com.kolor" %% "reactive-docker" % "0.1-SNAPSHOT"```

Sample Usage
==================
```
import com.kolor.docker.api._
import com.kolor.docker.api.json.Formats._
import scala.concurrent.ExecutionContext.Implicits.global


implicit val docker = Docker("localhost")

val maybeImages = docker.images()
val maybeContainers = docker.containers()

for {
	images <- maybeImages
	containers <- maybeContainers} yield {
	images.map(i => println(s"Image: $i"))	containers.map(c => println(s"Container: $c"))}

```

create a new container from busybox image and start it:

```
import com.kolor.docker.api._
import com.kolor.docker.api.json.Formats._
import scala.concurrent.duration.Duration
import scala.concurrent.Await
import play.api.libs.iteratee._
import scala.concurrent.ExecutionContext.Implicits.global


implicit val docker = Docker("localhost")
val timeout = Duration.create(30, SECONDS)

val cmd = Seq("/bin/sh", "-c", "while true; do echo hello world; sleep 1; done")
val containerName = "reactive-docker"
val imageTag = RepositoryTag.create("busybox", Some("latest"))
val cfg = ContainerConfig("busybox", cmd)

// create enumerator to consume & parse stream of docker status messages
val (it, en) = Concurrent.joined[Array[Byte]]
val maybeMessages = (en &> DockerEnumeratee.statusStream() |>>> Iteratee.getChunks)

// create image and collect status messages
Await.result(docker.imageCreate(imageTag)(it).flatMap(_.run), timeout)

// collect & print status messages
val messages = Await.result(maybeMessages, timeout)
messages.map(m => println(s"imageCreate: $m"))

// create container
val containerId = Await.result(docker.containerCreate("busybox", cfg, Some(containerName)), timeout)._1

// run container
Await.result(docker.containerStart(containerId), timeout)
println(s"container $containerId is running")
```


For more and up2date examples see ``DockerQuickSpec.scala`` and ``DockerApiSpec.scala`` in the ``test/`` folder