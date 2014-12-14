package com.kolor.docker.api.entities

import scala.io.BufferedSource


class ConfigReader(config: BufferedSource) {
  private lazy val lines = config.getLines()

  def getProperty(name: String): String =
    lines
      .find(_.startsWith(s"$name="))
      .map(_.replace(s"$name=", ""))
      .getOrElse("")
}
