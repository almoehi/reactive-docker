package com.kolor.docker

package object dsl {

  import scala.language.implicitConversions
  
  implicit def shell(args:String*) = new ShellWrapper(args:_*)
  implicit def bash(args:String*) = new BashWrapper(args:_*)
  implicit def sh(args:String*) = new ShWrapper(args:_*)
  implicit def ksh(args:String*) = new KshWrapper(args:_*)
  implicit def args(args:String*) = new ArgsWrapper(args:_*)
  implicit def withArgs(args:String*) = new ArgsWrapper(args:_*)
}