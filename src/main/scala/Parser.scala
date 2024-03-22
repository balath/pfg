package generator

import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.util.{Failure, Random, Success, Try, control}
import common.DataRegex._
import dsl._
import model._

object Parser extends IOApp:

  val r = new Random()

  extension(cfs: Vector[ChordFigure])
    def zipWithOcurrences: Map[ChordFigure, Double] = cfs.zipWithOcurrences

  override def run(args: List[String]): IO[ExitCode] = program

  val program = for {
    lines <- readFromRawData(dataPath)
    chorales <- extractChorales(lines)
    (majorChorales, minorChorales): (Vector[Choral], Vector[Choral]) = chorales.partition(_.mode.equals(Mode.maj))
    majorModel <- processModel(majorChorales)
    minorModel <- processModel(minorChorales)
    _ <- IO.println(s"${chorales.length} chorales parsed successfully")
      >> IO.println(s"Major semiphrase simulation: ${majorModel.generateChoral(r).mkString(";")}")
      >> IO.println(s"Minor semiphrase simulation: ${minorModel.generateChoral(r)}")
  } yield ExitCode.Success

  def readFromRawData(path: String): IO[Vector[String]] =
    Resource
      .fromAutoCloseable(IO(scala.io.Source.fromFile(path)))
      .use(reader => IO(reader.getLines.toVector))

  def extractChorales(lines: Vector[String]): IO[Vector[Choral]] =
    val chorales: Vector[Choral] = lines.map(line =>
      val (metadata, chords) = line match {
        case chordAndMetadataRegex(m, c) => (m, c)
      }
      val metadataArray = metadata.split(",")

      //Checker for chord annotation errors
      val wrongChords = chords.split(",|;").find(!chordRegex.matches(_))
      if wrongChords.isDefined then
        throw new NumberFormatException(s"Not valid chord symbol ${wrongChords.get} at choral ${metadataArray(0)}")

      val semiphrases: Vector[Semiphrase] = chords.split(";").map(_.split(",").map(c =>
        val enumChord = Try(ChordFigure.valueOf(c))
        enumChord match
          case Success(value) => value
          case Failure(exception) =>
            println(exception.getMessage)
            ChordFigure.Empty
      ).toVector).toVector
      val semiphrasesQuantity = semiphrases.length
      val choralNum = metadataArray(0).toInt
      val key = Note.valueOf(metadataArray(1))
      val mode = Mode.valueOf(metadataArray(2))

      Choral(choralNum, key, mode, semiphrases)
    )
    IO(chorales)

  //Model processing functions ---------------------------------------------------------------------------------
  def processModel(chorales: Vector[Choral]): IO[Model] =
    val firstSemiphrasesModel = processSemiphrases(chorales.map(_.semiphrases.head))
    val middleSemiphrasesModel = processSemiphrases(chorales.flatMap(_.semiphrases.tail.dropRight(1)))
    val lastSemiphrasesModel = processSemiphrases(chorales.map(_.semiphrases.last))
    val endingToInitialChordsTransitions: FirstOrderTransitions = chorales
      .flatMap(choral => choral.semiphrases.lazyZip(choral.semiphrases.tail).map((a, b) => (a.last, b.head)))
      .groupMapReduce((cf1, _) => cf1)((_, cf2) => Vector(cf2))(_ ++ _).toVector
      .map(chordProgressionsToTransitions).toMap
    val (minSemiphrases, maxSemiphrases) = chorales
      .map(_.semiphrases.length)
      .foldLeft((Int.MaxValue, Int.MinValue)) {
        case ((accMin, accMax), length) => (accMin.min(length), accMax.max(length))
      }
    println(s"Model ${chorales.head.mode} :min = $minSemiphrases ; max = $maxSemiphrases")



    IO(Model(firstSemiphrasesModel, middleSemiphrasesModel, lastSemiphrasesModel, endingToInitialChordsTransitions))

  def processSemiphrases(semiphrases: Vector[Semiphrase]): SemiphraseModel =
    val cf1Ocurrences = semiphrases.map(_.head).zipWithOcurrences
    val cf1SelectionWheel: SelectionWheel = chordOcurrencesToSelectionWheel(cf1Ocurrences.toVector, semiphrases.length)

    val cf2Transitions: FirstOrderTransitions = semiphrases
      .map(sp => (sp(0), sp(1)))
      .groupMapReduce((cf1, _) => cf1)((_, cf2) => Vector(cf2))(_ ++ _).toVector
      .map(chordProgressionsToTransitions).toMap

    val transitions: SecondOrderTransitions = semiphrases
      .flatMap(sp => sp.lazyZip(sp.tail).lazyZip(sp.tail.tail))
      .groupMapReduce((cf1, _, _) => cf1)((_, cf2, cf3) => Vector((cf2, cf3)))(_ ++ _).toVector
      .map((cf1, cfs) =>
        val subMap = cfs
          .groupMapReduce((cf2, _) => cf2)((_, cf3) => Vector(cf3))(_ ++ _).toVector
          .map(chordProgressionsToTransitions).toMap
        cf1 -> subMap
      ).toMap

    val endingChords: Map[ChordFigure, Double] = semiphrases
      .map(_.last)
      .zipWithOcurrences
      .map((cf, occurrences) => (cf, occurrences / semiphrases.length))

    val (lengthsSum, maxLength): (Int, Int) = semiphrases.foldLeft((0, 0))((z, sp) => (z._1 + sp.length, sp.length.max(z._2)))
    val averageLength = lengthsSum / (1.0 * semiphrases.length)

    SemiphraseModel(cf1SelectionWheel, cf2Transitions, transitions, endingChords, averageLength, maxLength)

  /**
   * Given a tuple of a chord and its next chords in a progression, generates the transitions tuple for such chord
   * by counting ocurrences and generating a selection wheel.   *
   *
   * @param progressions tuple of a chord and a collection of its possible next chords
   * @return tuple of a chord and its selection wheel of transitions
   */
  def chordProgressionsToTransitions(progressions: (ChordFigure, Vector[ChordFigure])): (ChordFigure, SelectionWheel) =
    val cf2sOccurrences: Vector[(ChordFigure, Double)] = progressions._2.zipWithOcurrences.toVector
    val cf2sSelectionWheel: SelectionWheel = chordOcurrencesToSelectionWheel(cf2sOccurrences, progressions._2.length)
    progressions._1 -> cf2sSelectionWheel

  /**
   * Given a vector of chords figures with its occurrences, generates a "roulette wheel" as used in Fitness
   * Proportionate Selection algorithm (FPS), used in genetic algorithms for selecting chromosomes.
   * * There are faster alternatives, as "alias method"
   *
   * @param chordOccurrences Vector of tuples with a chord and its occurrences
   * @param totalOccurrences Sum of all chords occurrences
   * @return A roulette wheel with the chord proportionate distribution in a range [0,~1)
   */
  def chordOcurrencesToSelectionWheel(chordOccurrences: Vector[(ChordFigure, Double)], totalOccurrences: Int): SelectionWheel =
    def rec(previousMark: Double, acc: Vector[(Double, ChordFigure)], chordsOccurrences: Vector[(ChordFigure, Double)]): SelectionWheel =
      chordsOccurrences match
        case (cf, occurrences) +: tail =>
          val newMark: Double = previousMark + (occurrences / totalOccurrences)
          rec(newMark, acc :+ (newMark, cf), tail)
        case _ => acc

    rec(0.0f, Vector.empty, chordOccurrences)



//  def generateTransitionMatrixFromSemiphrases(value: Vector[Semiphrase]): TransitionMatrix = ???