package model

import cats.kernel.Previous
import dsl.{ChordFigure, Mode, Note}

import scala.util.{Failure, Random, Success, Try}

type SelectionTuple = (Double, ChordFigure)
type SelectionWheel = Vector[SelectionTuple]
type FirstOrderTransitions = Map[ChordFigure, SelectionWheel]
type SecondOrderTransitions = Map[ChordFigure, Map[ChordFigure, SelectionWheel]]

case class SemiphraseModel(cf1Distribution: SelectionWheel,
                           cf2Transitions: FirstOrderTransitions,
                           transitions: SecondOrderTransitions,
                           endingChords: Map[ChordFigure, Double],
                           averageLength: Double,
                           maxLength: Int
                          )

case class Model(initialSemiphrase: SemiphraseModel,
                 middleSemiphrases: SemiphraseModel,
                 lastSemiphrase: SemiphraseModel,
                 endingToInitialChordsTransitions: FirstOrderTransitions
                 //                  middles: TransitionMatrix,
                 //                  last: TransitionMatrix
                ):
  def generateChoral(r: Random): Vector[Vector[ChordFigure]] =
    val initial = generateSemiphrases(initialSemiphrase, None, 1)(r)
    val middle1 = generateSemiphrases(middleSemiphrases, Some(initial.last), 1)(r)
    val middle2 = generateSemiphrases(middleSemiphrases, Some(middle1.last), 1)(r)
    val last = generateSemiphrases(lastSemiphrase, Some(middle2.last), 1)(r)
    Vector(initial, middle1, middle2, last)
  def genFirstChord(model: SemiphraseModel, previousSemiphraseEnding: Option[ChordFigure])(r: Random): ChordFigure =
    val selectionWheel = previousSemiphraseEnding match
      case Some(endingChord) =>
        println(s"Found distribution for $endingChord")
        endingToInitialChordsTransitions(endingChord)
      case None =>
        println("First semiphrase starting")
        model.cf1Distribution

    selectionWheel.find((prob, chord) => r.nextDouble() <= prob).getOrElse(model.cf1Distribution.last)._2

  def genSecondChord(model: SemiphraseModel, cf1: ChordFigure)(r: Random): ChordFigure =
    val cf2Distribution = Try(model.cf2Transitions(cf1))
    cf2Distribution match
      case Success(cf2Distribution) => cf2Distribution.find((prob, chord) => r.nextDouble() <= prob).getOrElse(cf2Distribution.last)._2
      case Failure(e) =>
        println(s"Failure at looking for second chord, looking at final semiphrase")
        ChordFigure.Empty
        //TODO Fix link between last middle semiphrase and final semiphrase

  def endSemiphrase(model: SemiphraseModel, currentLength: Int, currentChordEndingProbability: Double)(r: Random): Boolean =
    if currentChordEndingProbability == 1.0 then return true
    if currentLength >= model.maxLength && currentChordEndingProbability != 0.0 then return true
    val currentLengthBias = (currentLength / model.averageLength) * currentChordEndingProbability
    val random = r.nextDouble()
    //    println(s"($currentLength): chordProb = $currentChordEndingProbability - bias = $currentLengthBias - r = $random")
    if random < currentLengthBias then return true
    false

  def generateSemiphrases(model: SemiphraseModel, previousSemiphraseEnding: Option[ChordFigure], quantity: Int)(r: Random): Vector[ChordFigure] =
    def rec(previousChords: (ChordFigure, ChordFigure), acc: Vector[ChordFigure], end: Boolean): Vector[ChordFigure] = end match
      case true => acc
      case false =>
        val nextChordDistribution = Try(model.transitions(previousChords._1)(previousChords._2))
        nextChordDistribution match
          case Failure(e) =>
            println(s"Ending by failure: ${e.getMessage} (previousChords: $previousChords)")
            acc
          case Success(nCD) =>
            val nextChord = nCD.find((prob, chord) => r.nextDouble() <= prob).getOrElse(nCD.last)._2
//            print(s"Ending with $nextChord ")
            val endNow = endSemiphrase(model, acc.size + 1, model.endingChords.getOrElse(nextChord, 0.0))(r)
            rec((previousChords._2, nextChord), acc :+ nextChord, endNow)

//    println(s"First semiphrase length = ${model.averageLength}")
    val cf1 = genFirstChord(model, previousSemiphraseEnding)(r)
    val cf2 = genSecondChord(model, cf1)(r)
    rec((cf1, cf2), Vector(cf1, cf2), false)
  
end Model
