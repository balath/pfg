package generator

import cats.effect.{ExitCode, IO, IOApp, Resource}
import dsl.{ChordFigure, Mode, Note}
import common.Common.*

import scala.util.{Failure, Success, Try, control}

object Parser extends IOApp:

  type Semiphrase = Vector[ChordFigure]
  case class Choral(num: Int, key: Note, mode: Mode, first: Semiphrase, middle: Vector[Semiphrase], last: Semiphrase)

  val program = for {
    lines <- readFromRawData(dataPath)
    chorals <- extractChorals(lines)
    _ <- IO.println(s"${chorals.size} chorals parsed successfully")
  } yield ExitCode.Success

  override def run(args: List[String]): IO[ExitCode] = program

  def readFromRawData(path: String): IO[Vector[String]] =
    Resource
      .fromAutoCloseable(IO(scala.io.Source.fromFile(path)))
      .use(reader => IO(reader.getLines.toVector))

  def extractChorals(lines: Vector[String]): IO[Vector[Choral]] =
    val chorals: Vector[Choral] = lines.map(line =>
      val (metadata,chords) = line match { case chordMetadataSplitter(m,c) => (m,c) }

      //Checker for chord annotation errors
      val wrongChords = chords.split(",|;").find(!chordRegex.matches(_))
      if wrongChords.isDefined then
      throw new NumberFormatException(s"Not valid chord symbol ${wrongChords.get} at choral ${metadata.takeWhile(!_.equals(',')).drop(1)}")

      val semiphrases: Vector[Semiphrase] = chords.split(";").map(_.split(",").map(c =>
        val enumChord = Try(ChordFigure.valueOf(c))
        enumChord match
          case Success(value) => value
          case Failure(exception) =>
            println(exception.getMessage)
            ChordFigure.i
      ).toVector).toVector
      val semiphrasesQuantity = semiphrases.size
      val firstSemiphrase = semiphrases.head
      val lastSemiphrase = semiphrases.last
      val middleSemiphrases = semiphrases.tail.dropRight(1)

      val metadataArray = metadata.replaceAll("""\{|\}""","").toLowerCase.split(",")
      val choralNum = metadataArray(0).toInt
      val key = Note.valueOf(metadataArray(1))
      val mode = Mode.valueOf(metadataArray(2))
      Choral(choralNum, key, mode, firstSemiphrase,middleSemiphrases,lastSemiphrase)
    )
    IO(chorals)

