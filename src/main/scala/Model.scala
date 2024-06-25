package model

import cats.kernel.Previous
import dsl.{Choral, Chord, ChordFigure, GeneratedChoral, Mode, Note}
import scala.util.{Failure, Random, Success, Try}

val LOG_ENABLED = true

type SelectionTuple = (Double, ChordFigure)
type SelectionWheel = Vector[SelectionTuple]
type SimpleStateTransitions = Map[ChordFigure, SelectionWheel]
type CompoundStateTransitions = Map[ChordFigure, SimpleStateTransitions]

extension (sw: SelectionWheel)
  def toDataStructureCode: String =
    s"Vector(${
      sw.map {
        case (d, c) => s"($d, $c)"
      }.mkString(",")
    })"
extension (fot: SimpleStateTransitions)
  def fotToDataStructureCode: String =
    s"""Map(${
      fot.map {
        case (cf, sw) => s"$cf -> Vector(${
          sw.map {
            case (d, c) => s"($d, $c)"
          }.mkString(",")
        })"
      }.mkString(",")
    })"""

extension (cfd: Map[ChordFigure, Double])
  def toDataStructureCode: String =
    s"Map(${
      cfd.map {
        case (cf, d) => s"($cf -> $d)"
      }.mkString(",")
    })"

extension (sot: Map[ChordFigure, Map[ChordFigure, SelectionWheel]])
  def sotToDataStructureCode: String =
    s"""Map(${sot.map {
      case (cf, fot) => s"$cf -> ${fot.fotToDataStructureCode}"
    }.mkString(",")}
    )"""

extension (r: Random)
  def nSigma: Int = 3
  def nextGaussianBetween(lowerBound: Double, upperBound: Double): Double =
    val mean = ((upperBound - lowerBound) / 2) + lowerBound
    val std = (mean - lowerBound) / nSigma
    lowerBound.max(upperBound.min(r.nextGaussian() * std + mean))
  def nextGaussianBetween(lowerBound: Int, upperBound: Int): Int =
    val mean = ((upperBound - lowerBound) / 2.0) + lowerBound
    val std = (mean - lowerBound) / nSigma
    lowerBound.max(upperBound.min((r.nextGaussian() * std + mean).round.toInt))

case class SemiphraseModel(cf1Distribution: SelectionWheel,
                           cf2Transitions: SimpleStateTransitions,
                           transitions: CompoundStateTransitions,
                           endingChords: Map[ChordFigure, Double],
                           averageLength: Double,
                           minLength: Int,
                           maxLength: Int
                          ):
  def toDataStructureCode: String =
    s"""SemiphraseModel(
      ${cf1Distribution.toDataStructureCode},
      ${cf2Transitions.fotToDataStructureCode},
      ${transitions.sotToDataStructureCode},
      ${endingChords.toDataStructureCode},
      $averageLength,
      $minLength,
      $maxLength
      )"""

/**
 *
 * @param initialSemiphrase
 * @param middleSemiphrases
 * @param lastSemiphrase
 * @param endingToInitialChordsTransitions
 * @param middleSectionBounds
 */
@SerialVersionUID(123L)
case class Model(initialSemiphrase: SemiphraseModel,
                 middleSemiphrases: SemiphraseModel,
                 lastSemiphrase: SemiphraseModel,
                 endingToInitialChordsTransitions: SimpleStateTransitions,
                 middleSectionBounds: (Int, Int)
                 //                  middles: TransitionMatrix,
                 //                  last: TransitionMatrix
                ) extends Serializable:
  def generateChoral(r: Random, key: Note, mode: Mode): GeneratedChoral =
    def generateMiddleSection(length: Int, acc: Vector[Vector[ChordFigure]], lastChord: ChordFigure): Vector[Vector[ChordFigure]] =
      if length == 0 then acc
      else
        val currentSemiphrase = generateSemiphrases(middleSemiphrases, Some(lastChord))(r)
        if LOG_ENABLED then
          println(s"Semiphrase ${acc.length + 1} generated: $currentSemiphrase")
        generateMiddleSection(length - 1, acc :+ currentSemiphrase, currentSemiphrase.last)

    val initial = generateSemiphrases(initialSemiphrase, None)(r)
    if LOG_ENABLED then
      println(s"Initial semiphrase generated: $initial")
    val middleSectionLength = r.nextGaussianBetween(middleSectionBounds._1, middleSectionBounds._2)
    val middleSection = generateMiddleSection(middleSectionLength, Vector.empty, initial.last)
    val last = generateSemiphrases(lastSemiphrase, Some(middleSection.last.last))(r)
    if LOG_ENABLED then
      println(s"Final semiphrase generated: $last")

    val choral = initial +: middleSection :+ last
    GeneratedChoral(key, mode, choral map (_ map (
      key chord _
      )))

  def genFirstChord(model: SemiphraseModel, previousSemiphraseEnding: Option[ChordFigure])(r: Random): ChordFigure =
    previousSemiphraseEnding match
      case Some(endingChord) =>
        endingToInitialChordsTransitions(endingChord)
          .filter((_, chord) => model.cf1Distribution.exists(_._2.equals(chord)))
          .find((prob, chord) => r.nextDouble() <= prob)
          .getOrElse(model.cf1Distribution.last)._2
      case None =>
        model.cf1Distribution
          .find((prob, chord) => r.nextDouble() <= prob)
          .getOrElse(model.cf1Distribution.last)._2

  def genSecondChord(model: SemiphraseModel, cf1: ChordFigure)(r: Random): ChordFigure =
    val cf2Distribution = model.cf2Transitions(cf1)
    cf2Distribution.find((prob, chord) => r.nextDouble() <= prob).getOrElse(cf2Distribution.last)._2

  def endSemiphrase(semiphraseModel: SemiphraseModel, currentLength: Int, currentChordEndingProbability: Double, cf: ChordFigure)(r: Random): Boolean =
    if currentChordEndingProbability == 1.0 then
      if LOG_ENABLED then
        println(s"ENDING at ($currentLength) by ONLY ENDING CHORD with $cf")
      return true
    if currentLength >= semiphraseModel.maxLength && currentChordEndingProbability > 0.0 then
      if LOG_ENABLED then
        println(s"ENDING at ($currentLength) by LENGTH OUT OF BOUNDS with $cf")
      return true
    val currentLengthBias = currentLength / semiphraseModel.averageLength
    val endingBias = currentLengthBias * currentChordEndingProbability
    val random = r.nextGaussianBetween(0.0, 1.0)
    if random < endingBias then
      if LOG_ENABLED then
        println(s"ENDING at ($currentLength) by STOCHASTIC ENDING with $cf, chordProb=$currentChordEndingProbability, lengthProb=$currentLengthBias, bias=$endingBias, r=$random")
      return true
    false

  def generateSemiphrases(model: SemiphraseModel, previousSemiphraseEnding: Option[ChordFigure])(r: Random): Vector[ChordFigure] =
    def genChordFigures(previousChords: (ChordFigure, ChordFigure), acc: Vector[ChordFigure], end: Boolean): Vector[ChordFigure] = end match
      case true => acc
      case false =>
        val nextChordDistribution = Try(model.transitions(previousChords._1)(previousChords._2))
        nextChordDistribution match
          case Failure(e) =>
            if LOG_ENABLED then
              println(s"ENDING at (${acc.length}) by 2ND ORDER TRANSITION NOT FOUND with ${acc.last}")
            acc
          case Success(nCD) =>
            val nextChord = nCD.find((prob, chord) => r.nextDouble() <= prob).getOrElse(nCD.last)._2
            val endNow = endSemiphrase(model, acc.size + 1, model.endingChords.getOrElse(nextChord, 0.0), nextChord)(r)
            genChordFigures((previousChords._2, nextChord), acc :+ nextChord, endNow)

//    if logPrinting then
//    println(s"Semiphrase model lengths: min=${model.minLength}, max=${model.maxLength}")
    val cf1 = genFirstChord(model, previousSemiphraseEnding)(r)
    val cf2 = genSecondChord(model, cf1)(r)
    genChordFigures((cf1, cf2), Vector(cf1, cf2), false)

end Model
