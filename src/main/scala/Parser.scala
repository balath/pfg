package generator

import cats.effect.{ExitCode, IO, IOApp, Resource}

import scala.util.{Failure, Random, Success, Try, control}
import common.DataRegex.*
import dsl.*
import model.{Distribution, FirstOrderChain, Model, SecondOrderChain}

object Parser extends IOApp:

  val r = new Random()

  override def run(args: List[String]): IO[ExitCode] = program

  val program = for {
    lines <- readFromRawData(dataPath)
    chorals <- extractChorals(lines)
    //    metadata <- processMetadata(chorals)
    model <- processModel(chorals)
    _ <- IO.println(s"${chorals.size} chorals parsed successfully")
      >> IO.println(model.cf1Distribution.mkString("First------\n","\n","\n-------------"))
      >> IO.println(model.cf2Transitions.mkString("Second------\n","\n","\n-------------"))
      >> IO.println("\nTransitions----------------\n")
      >> IO.println(model.initialSemiphraseTransitions.map((cf,mp)=> s"\n$cf ------ \n${mp.mkString("\n")}"))
      >> IO.println(s"Semiphrase simulation: ${model.genInitialSemiphrase(r)}")
  } yield ExitCode.Success

  def readFromRawData(path: String): IO[Vector[String]] =
    Resource
      .fromAutoCloseable(IO(scala.io.Source.fromFile(path)))
      .use(reader => IO(reader.getLines.toVector))

  def extractChorals(lines: Vector[String]): IO[Vector[Choral]] =
    val chorals: Vector[Choral] = lines.map(line =>
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
            ChordFigure.i
      ).toVector).toVector
      val semiphrasesQuantity = semiphrases.size
      val firstSemiphrase = semiphrases.head
      val middleSemiphrases = semiphrases.tail.dropRight(1)
      val lastSemiphrase = semiphrases.last

      val choralNum = metadataArray(0).toInt
      val key = Note.valueOf(metadataArray(1))
      val mode = Mode.valueOf(metadataArray(2))

      Choral(choralNum, key, mode, firstSemiphrase, middleSemiphrases, lastSemiphrase)
    )
    IO(chorals)

  //Model processing functions ---------------------------------------------------------------------------------
  def processModel(chorals: Vector[Choral]): IO[Model] =
    val modelSize = chorals.size
    val firstSemiphrases = chorals.map(_.first)
    val middleSemiphrases = chorals.flatMap(_.middle)
    val lastSemiphrases = chorals.map(_.last)

    val cf1Probs: Vector[(ChordFigure, Double)] = firstSemiphrases
      .map(_.head)
      .groupMapReduce(identity)(_ => 1.0)(_ + _)
      .toVector
    val cf1CumulativeDistribution: Distribution = cumulateProbabilities(cf1Probs, modelSize)

    val cf2CumulativeDistribution: FirstOrderChain = firstSemiphrases
      .map(sp => (sp(0), sp(1)))
      .groupMapReduce((cf1, _) => cf1)((_, cf2) => Vector(cf2))(_ ++ _).toVector
      .map((cf1, cf2s) =>
        val cf2sProbs = cf2s.groupMapReduce(identity)(_ => 1.0)(_ + _).toVector
        val cf2sCumDistrib = cumulateProbabilities(cf2sProbs, cf2s.size)
        cf1 -> cf2sCumDistrib
      ).toMap

    val initialSemiphraseTransitions: SecondOrderChain = firstSemiphrases
      .flatMap(sp => sp.lazyZip(sp.tail).lazyZip(sp.tail.tail))
      .groupMapReduce((cf1,_,_) => cf1)((_,cf2,cf3) => Vector((cf2,cf3)))(_ ++ _).toVector
      .map((cf1,cfs) =>
        val subMap = cfs
          .groupMapReduce((cf2, _) => cf2)((_, cf3) => Vector(cf3))(_ ++ _).toVector
          .map((cf2, cf3s) =>
            val cf3sProbs = cf3s.groupMapReduce(identity)(_ => 1.0)(_ + _).toVector
            val cf3sCumDistrib = cumulateProbabilities(cf3sProbs, cf3s.size)
            cf2 -> cf3sCumDistrib
          ).toMap
        cf1 -> subMap
      ).toMap


    //    val firstSemiphraseTransitionMatrix = generateTransitionMatrixFromSemiphrases(chorals.map(_.first))
    //    val middleSemiphrasesTransitionMatrix = generateTransitionMatrixFromSemiphrases(chorals.flatMap(_.middle))
    //    val lastSemiphraseTransitionMatrix = generateTransitionMatrixFromSemiphrases(chorals.map(_.last))
    IO(Model(cf1CumulativeDistribution, cf2CumulativeDistribution, initialSemiphraseTransitions))

  def cumulateProbabilities(probabilitiesVector: Vector[(ChordFigure, Double)], modelSize: Int): Vector[(Double, ChordFigure)] =
    def rec(lastValue: Double, acc: Vector[(Double, ChordFigure)], tail: Vector[(ChordFigure, Double)]): Vector[(Double, ChordFigure)] = tail match
      case (cf, q) +: newTail =>
        val newValue = lastValue + (q / modelSize)
        rec(newValue, acc :+ (newValue, cf), newTail)
      case _ => acc

    rec(0.0f, Vector.empty, probabilitiesVector)



//  def generateTransitionMatrixFromSemiphrases(value: Vector[Semiphrase]): TransitionMatrix = ???