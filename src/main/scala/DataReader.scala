import scala.io.StdIn.readLine
import scala.io.Source.fromFile
import dsl.{ChordFigure, Mode, Note}

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import common.Common._

object DataReader extends App:

  println(s"Last choral data line added: ${fromFile(dataPath).getLines().toVector.last}")

  while true do {
    println("Write new choral data line:")
    val line = readLine
    if line.equals("exit") then System.exit(0)
    if checkLine(line) then
      val file = new File(dataPath)
      val writer = new BufferedWriter(new FileWriter(file,true))
      writer.write("\n" + line)
      writer.close()
      println("Choral data line added to file")
  }

  def checkLine(line: String): Boolean = line match
      case chordMetadataSplitter(m,c) =>
        if !metadataRegex.matches(m) then
          println("Wrong metadata format")
          return false
        val wrongChords = c.split(",|;").find(!chordRegex.matches(_))
        if wrongChords.isDefined then
          println(s"Not valid chord symbol: ${wrongChords.get}")
          return false
        true
      case _ =>
        println("Wrong format separating metadata and chords")
        false