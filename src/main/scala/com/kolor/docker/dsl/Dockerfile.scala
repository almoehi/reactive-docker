package com.kolor.docker.dsl

import scala.collection.immutable.Nil
import scala.collection.generic.CanBuildFrom
import org.apache.commons.compress.archivers.tar._
import java.util.zip.GZIPOutputStream
import java.io._
import org.apache.commons.compress.utils.IOUtils

sealed trait DockerfileCmd {
  val cmd: String
  val arg: List[String]
  val asArray = false
}

sealed abstract class Command(val args: String*) extends DockerfileCmd { 
    override val arg = args.filter(_.nonEmpty).toList
    override val asArray = false
    override def toString = arg match {
      case Nil => s"# empty command: $cmd"
      case _ => asArray match {
        case true => s"$cmd [" + arg.map(s => s""""$s",""").mkString(" ").stripSuffix(" ").stripSuffix(",") + "]"
        case false => s"$cmd ${arg.mkString(" ")}"
      }
    }
}

sealed abstract class CommandBinary(val op1: String, val op2: String) extends DockerfileCmd {
  override val arg = Seq(op1, op2).toList
  override val asArray = false
  override def toString = arg match {
      case Nil => s"# empty binary command: $cmd"
      case _ => s"$cmd ${arg.mkString(" ")}"
    }
}



sealed trait ShellCmdWrapper {
  val shell: String
  val params: List[String]
  override def toString = (shell ++ params).mkString(" ")
}

case class ShellWrapper(args: String*) extends ShellCmdWrapper {
  override val shell = ""
  override val params = List(args:_*)
}

case class BashWrapper(args: String*) extends ShellCmdWrapper {
  override val shell = "/bin/bash"
  override val params = List(shell, "-c") ++ args
}

case class ShWrapper(args: String*) extends ShellCmdWrapper {
  override val shell = "/bin/sh"
  override val params = List(shell, "-c") ++ args
}

case class KshWrapper(args: String*) extends ShellCmdWrapper {
  override val shell = "/bin/ksh"
  override val params = List(shell, "-c") ++ args
}

case class ArgsWrapper(args: String*) extends ShellCmdWrapper {
  override val shell = ""
  override val params = List(args:_*)
}




case class Dockerfile(baseImage: String, tag: Option[String] = None) {
  protected[dsl] val cmds = scala.collection.mutable.ListBuffer.empty[DockerfileCmd]
  protected val includeFileList = scala.collection.mutable.ListBuffer.empty[java.io.File]
  protected val excludePatternsList = scala.collection.mutable.ListBuffer.empty[String]
  
  implicit val dockerfile = this
  
  private def installApt(packages: String*) = s"apt-get install -y " + packages.mkString(" ")
  private def installYum(packages: String*) = s"yum -y " + packages.mkString(" ")
  
  cmds.append(new DockerfileDSL.from(baseImage, tag))
  
  override def toString = {
    cmds.map(_.toString).mkString("\n")
  }
  
  
  import scala.collection.JavaConversions._
  protected def getFileTree(f: File): Stream[File] =
        f #:: (if (f.isDirectory) f.listFiles().toStream.flatMap(getFileTree) 
               else Stream.empty)
  
  /**
   * dockerfile MAINTAINER
   */
  def by(name: String) = {
    cmds.append(new DockerfileDSL.maintainer(name))
    dockerfile
  }
  
  /**
   * dockerfile RUN args
   */
  def run(args: String*) = {
    cmds.append(new DockerfileDSL.run(args:_*))
    dockerfile
  }
  
  def run(wrapper: ShellWrapper) = {
    cmds.append(new DockerfileDSL.run(wrapper.params.mkString(" ")))
    dockerfile
  }
  
  def exec(args: String*) = {
    cmds.append(new DockerfileDSL.run(args.mkString(" ")))
    dockerfile
  }
  
  /**
   * install using local packageManager
   * gets wrapped in a cmd like: RUN apt -y install $packages 
   */
  def install(args: String*) = run(installApt(args:_*))
  
  /**
   * dockerfile CMD
   */
  def starting(kv: (String,ArgsWrapper)) = {
    val vargs = Seq(kv._1) ++ kv._2.params
    cmds.append(new DockerfileDSL.cmd(vargs:_*))
    dockerfile
  }
  
  def starting(bin: String) = {
    cmds.append(new DockerfileDSL.cmd(bin))
    dockerfile
  }
  
  def starting(shell: ShellCmdWrapper) = shell match {
    case sh:ShellWrapper =>
        cmds.append(new DockerfileDSL.cmdShell(sh.params:_*))
        dockerfile
    case sh:ArgsWrapper=>
        cmds.append(new DockerfileDSL.cmdShell(sh.params:_*))
        dockerfile
    case sh => 
      cmds.append(new DockerfileDSL.cmd(sh.params:_*))
      dockerfile
  }
  
  /**
   * dockerfile ENTRYPOINT
   */
  def entering(bin: String) = {
    cmds.append(new DockerfileDSL.entrypoint(bin))
    dockerfile
  }
  
  def entering(kv: (String,ArgsWrapper)) = {
    val vargs = Seq(kv._1) ++ kv._2.params
    cmds.append(new DockerfileDSL.entrypoint(vargs:_*))
    dockerfile
  }
  
  def entering(shell: ShellCmdWrapper) = shell match {
    case sh: ShellWrapper => 
      cmds.append(new DockerfileDSL.entrypointShell(sh.params:_*))
      dockerfile
    case sh: ArgsWrapper=> 
      cmds.append(new DockerfileDSL.entrypointShell(sh.params:_*))
      dockerfile
    case sh =>
      cmds.append(new DockerfileDSL.entrypoint(sh.params:_*))
      dockerfile
  }
  
  /**
   * dockerfile EXPOSE
   */
  def withPorts(ports: Int*) = {
    cmds.append(new DockerfileDSL.expose(ports.map(_.toString):_*))
    dockerfile
  }
  
  def expose(ports: Int*) = withPorts(ports:_*)
  
  /**
   * dockerfile ENV
   */
  def $(kv: (String,String)) = {
    cmds.append(new DockerfileDSL.env(kv._1, kv._2))
    dockerfile
  }
  
  def env(kv: (String,String)) = $(kv)
  
  /**
   * dockerfile ADD src dest
   */
  def add(kv: (String,String)) = {
    val f = new java.io.File(kv._1)
    
    f.isDirectory match {
      case false => includeFileList.append(f)
      case _ => includeFileList.appendAll(getFileTree(f).filter(_.isFile))
    }
    
    cmds.append(new DockerfileDSL.add(kv._1, kv._2))
    dockerfile
  }
  
  
  /**
   * dockerfile COPY src dest
   */
  def copy(kv: (String,String)) = {
    val f = new java.io.File(kv._1)
    f.isDirectory match {
      case false => includeFileList.append(f)
      case _ => includeFileList.appendAll(getFileTree(f).filter(_.isFile))
    }
    
    cmds.append(new DockerfileDSL.copy(kv._1, kv._2))
    dockerfile
  }

  
  /**
   * dockerfile VOLUME
   */
  def withVolume(vol: String*) = {
    cmds.append(new DockerfileDSL.volume(vol:_*))
    dockerfile
  }
  
  def volume(vol: String*) = withVolume(vol:_*)
  
  /**
   * dockerfile USER $user
   */
  def as(user: String) = {
    cmds.append(new DockerfileDSL.user(user))
    dockerfile
  }
  
  /**
   * dockerfile WORKDIR $path
   */
  def cwd(path: String) = {
    cmds.append(new DockerfileDSL.workdir(path))
    dockerfile
  }
  
  /**
   * dockerfile ONBUILD $dockerCmd
   */
  def trigger(fn: () => Seq[DockerfileCmd]) = {
    // TODO: implement me
    ???
  }
  
  def exclude(pattern: String) = {
    excludePatternsList.append(pattern)
    dockerfile
  }
  
  
  def asTempfile: java.io.File = {
    val temp = java.io.File.createTempFile("dockerfile-dsl", "dockerfile")
    temp.deleteOnExit
    asTar(temp)
  }
  
  def asTar(out: String): java.io.File = asTar(new java.io.File(out))
  
  def asTar(file: java.io.File): java.io.File = {
    // TODO: use Futures here ?
    val dockerfileString = toString()
    
    var tarOs: TarArchiveOutputStream = null
    try {
           tarOs = new TarArchiveOutputStream(
                new GZIPOutputStream(
                     new BufferedOutputStream(new FileOutputStream(file))))
           
           // add Dockerfile
           val dockerfileEntry = new TarArchiveEntry("Dockerfile")
           dockerfileEntry.setSize(dockerfileString.getBytes().length)
           tarOs.putArchiveEntry(dockerfileEntry)
           IOUtils.copy(new ByteArrayInputStream(dockerfileString.getBytes), tarOs)
           tarOs.closeArchiveEntry
           
           // add .dockerignore file
           excludePatternsList.isEmpty match {
             case true => // ignore
             case false => 
               val dockerignoreEntry = new TarArchiveEntry(".dockerignore")
               val dotIgnore = excludePatternsList.mkString("\n")
               dockerignoreEntry.setSize(dotIgnore.getBytes().length)
               tarOs.putArchiveEntry(dockerignoreEntry)
               IOUtils.copy(new ByteArrayInputStream(dotIgnore.getBytes), tarOs)
               tarOs.closeArchiveEntry
           }
           
           // add included files & dirs
           includeFileList.map{f => 
             //println(s"[+] ${f.getPath}")
             val entry = new TarArchiveEntry(f, f.getPath())
             tarOs.putArchiveEntry(entry)
             IOUtils.copy(new BufferedInputStream(new FileInputStream(f)), tarOs)
             tarOs.closeArchiveEntry
           }
           
      } finally {
           if (tarOs != null) {
              tarOs.finish
              tarOs.close
           }
      }
      
      file
  }
}

/**
 * companion object
 */
case object Dockerfile {
  def from(repo: String) = new Dockerfile(repo)
  def from(repo: String, tag: String) = new Dockerfile(repo, Some(tag))
}

object DockerfileDSL {
  
  case class from(repo: String, tag: Option[String] = None) extends Command(s"$repo:${tag.getOrElse("latest")}") {
    override val cmd = "FROM"
  }
  
  case class maintainer(name: String) extends Command(name) {
    override val cmd = "MAINTAINER"
  }
  
  case class run(cmds: String*) extends Command(cmds:_*) {
    override val cmd = "RUN"
    override val asArray = cmds match {
      case Nil => false
      case s if (s.length == 1) => false
      case xs => true
    }
  }
  
  case class expose(cmds: String*) extends Command(cmds:_*) {
    override val cmd = "EXPOSE"
  }
  
  case class volume(cmds: String*) extends Command(cmds:_*) {
    override val cmd = "VOLUME"
  }
  
  case class user(cmds: String*) extends Command(cmds.headOption.getOrElse("")) {
    override val cmd = "USER"
  }
  
  case class workdir(cmds: String*) extends Command(cmds.headOption.getOrElse("")) {
    override val cmd = "WORKDIR"
  }
  
  case class add(src: String, dest: String) extends CommandBinary(src, dest) {
    override val cmd = "ADD"
  }
  
  case class copy(src: String, dest: String) extends CommandBinary(src, dest) {
    override val cmd = "COPY"
  }
  
  case class env(env: String, value: String) extends CommandBinary(env, value) {
    override val cmd = "ENV"
  }
  
  case class cmd(params: String*) extends Command(params:_*) {
    override val cmd = "CMD"
    override val asArray = true
  }
  
  case class cmdShell(params: String*) extends Command(params:_*) {
    override val cmd = "CMD"
    override val asArray = false
  }
  
  case class entrypoint(params: String*) extends Command(params:_*) {
    override val cmd = "ENTRYPOINT"
    override val asArray = true
  }
  
  case class entrypointShell(params: String*) extends Command(params:_*) {
    override val cmd = "ENTRYPOINT"
    override val asArray = false
  }
  
  
}

