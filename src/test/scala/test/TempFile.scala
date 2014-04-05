package test


import java.io.{File, FileNotFoundException, BufferedOutputStream, FileOutputStream}
import play.api.libs.iteratee.Iteratee
import java.io.InputStream
import java.io.OutputStream

/**
 * taken from: https://github.com/twitter/util/blob/master/util-core/src/main/scala/com/twitter/io/TempFile.scala
 */
object TempFile {
  /**
   * Create a temporary file from the given (resource) path. The
   * tempfile is deleted on JVM exit.
   *
   * Note, due to the usage of `File.deleteOnExit()` callers should
   * be careful using this as it can leak memory.
   * See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4513817 for
   * example.
   *
   * @param path the resource-relative path to make a temp file from
   * @return the temp File object
   */
  def fromResourcePath(path: String): File = fromResourcePath(getClass, path)

  /**
   * Create a temporary file from the given (resource) path. The
   * tempfile is deleted on JVM exit.
   *
   * Note, due to the usage of `File.deleteOnExit()` callers should
   * be careful using this as it can leak memory.
   * See http://bugs.sun.com/bugdatabase/view_bug.do?bug_id=4513817 for
   * example.
   *
   * @param klass the `Class` to use for getting the resource.
   * @param path the resource-relative path to make a temp file from
   * @return the temp File object
   */
  def fromResourcePath(klass: Class[_], path: String): File = {
    val (basename, ext) = {
      val last = path.split(File.separatorChar).last
      last.split('.').reverse match {
        case Array(basename) =>
          (basename, "")
        case Array(ext, base@_*) =>
          (base.reverse.mkString("."), ext)
      }
    }

    klass.getResourceAsStream(path) match {
      case null =>
        throw new FileNotFoundException(path)
      case stream =>
        val file = File.createTempFile(basename, "." + ext)
        file.deleteOnExit()
        val fos = new BufferedOutputStream(new FileOutputStream(file), 1<<20)
        copy(stream, fos)
        fos.flush()
        fos.close()
        stream.close()
        file
    }
  }
  
  final private def copy(
    inputStream: InputStream,
    outputStream: OutputStream,
    bufferSize: Int = 1024
  ) {
    val buf = new Array[Byte](bufferSize)
    inputStream.read(buf, 0, buf.size) match {
      case -1 => ()
      case n =>
        outputStream.write(buf, 0, n)
        copy(inputStream, outputStream, bufferSize)
    }
  }
  
  def create(s: String) = {
    val dir = System.getProperty("java.io.tmpdir")
    java.io.File.createTempFile(s, "tmp")
  }
}
