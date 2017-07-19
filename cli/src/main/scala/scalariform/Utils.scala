package scalariform

object Utils {
  def writeText(file: java.io.File, text: String, encodingOpt: Option[String] = None) {
    import java.io.{OutputStreamWriter, FileOutputStream}
    val encoding = encodingOpt getOrElse (System getProperty "file.encoding")
    val writer = new OutputStreamWriter(new FileOutputStream(file), encoding)
    try
      writer.write(text)
    finally
      writer.close()
  }
}

