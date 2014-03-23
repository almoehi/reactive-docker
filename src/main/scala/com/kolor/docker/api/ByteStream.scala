package com.kolor.docker.api

import com.ning.http.client._
import com.ning.http.util.AsyncHttpProviderUtils.parseCharset
import play.api.libs.iteratee.Enumerator
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import play.api.libs.iteratee.Concurrent
import com.ning.http.client
import org.slf4j.LoggerFactory
import play.api.libs.iteratee.Iteratee
import scala.concurrent.Promise
import dispatch.StatusCode

sealed class Bytes extends AsyncHandler[Unit] {
  import AsyncHandler.STATE._

  @volatile private var charset = "utf-8"
  @volatile private var state = CONTINUE
  
  private  val (en, channel) = Concurrent.broadcast[Array[Byte]]
  
  private class AbortOnIterateeDone() extends RuntimeException
  private class AbortOnCancelation() extends RuntimeException
  
  private val logger = LoggerFactory.getLogger(getClass())
  
   val promiseStatus = Promise[Int]()
   val promiseHeader = Promise[HttpResponseHeaders]()
  
  def onThrowable(t: Throwable) {
    t match {
    	case _: AbortOnIterateeDone => logger.info(s"WS call aborted on purpose : $t")
    	case _ => {
    		logger.error("Exception, closing enumerator channel and leaking exception", t)
    		try {
		      channel.eofAndEnd()
		    } catch {
		      case t:Throwable => logger.error("failed to close channel", t)
		    }
    		throw t
    	}
    }
  }
  
  def onCompleted(): Unit = {
    logger.debug("Closing channel as WS call is completed")
    try {
      channel.eofAndEnd()
    } catch {
      case t:Throwable => logger.error("failed to close channel", t)
    }
    ()
  }
  def enumerate: Enumerator[Array[Byte]] = en
  def onStatusReceived(status: HttpResponseStatus) = {
    logger.debug("Received Status Code: " + status.getStatusCode())
    
    if (status.getStatusCode() >= 300) {
      throw new StatusCode(status.getStatusCode())
    } else {
      promiseStatus.success(status.getStatusCode())
    }
    state
  }
  
  def onHeadersReceived(headers: HttpResponseHeaders) = {
    for {
      ct <- Option(headers.getHeaders.getFirstValue("content-type"))
      cs <- Option(parseCharset(ct))
    } charset = cs
    
    logger.debug("Received headers: " + headers.getHeaders().toString())
    promiseHeader.success(headers)
    state
  }
  
  def onBodyPartReceived(bodyPart: HttpResponseBodyPart) = {
    if (state == CONTINUE) {
      // read & convert from tranfer charset to uft-8
      val str = new String(bodyPart.getBodyPartBytes(), charset)
      logger.debug(s"onBodyPartReceived(${str.trim})")
      channel.push(str.trim().getBytes("utf-8"))
    }
    state
  }
  
  def stop() {
    state = ABORT
    logger.debug("Closing channel as WS call is aborted")
    try {
      channel.eofAndEnd()
    } catch {
      case t:Throwable => logger.error("failed to close channel", t)
    }
  }
}


object ByteStream {

  def apply: Bytes = new Bytes 
}