import scala.io.StdIn.readLine
import scala.io.Source.fromFile
import dsl.{ChordFigure, Mode, Note}

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}
import common.Values.*

import scala.util.{Failure, Success, Try}

object DataReader extends App:

  val file = fromFile(dataPath)
  println(s"Last choral data line added: ${file.getLines().toVector.last}")
  file.close()
  while true do {
    println("Write new choral data line:")
    val line = readLine
    if line.equals("exit") then System.exit(0)
    if checkLine(line) then
      val file = new File(dataPath)
      val writer = new BufferedWriter(new FileWriter(file, true))
      writer.write("\n" + line)
      writer.close()
      println("Choral data line added to file")
  }

  def checkLine(line: String): Boolean = line match
    case chordAndMetadataRegex(m, c) =>
      if !metadataRegex.matches(m) then
        println("Wrong metadata format")
        return false
      val metadata = m.split(",").map(s => Try(s.toInt).getOrElse(-1))
      if metadata(5) > metadata(3) || metadata(6) > metadata(3) then
        println("Measures length out of time signature")
        return false
      val wrongChords = c.split("[,;]").find(!chordRegex.matches(_))
      wrongChords match
        case Some(wrongChord) =>
          println(s"Not valid chord symbol: $wrongChord")
          false
        case None => true
    case _ =>
      println("Wrong format separating metadata and chords")
      false