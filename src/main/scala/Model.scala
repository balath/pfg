package model

import cats.kernel.Previous
import dsl.{ChordFigure, Mode, Note}

import scala.util.{Failure, Random, Success, Try}

type SelectionTuple = (Double, ChordFigure)
type SelectionWheel = Vector[SelectionTuple]
type FirstOrderTransitions = Map[ChordFigure, SelectionWheel]
type SecondOrderTransitions = Map[ChordFigure, Map[ChordFigure, SelectionWheel]]

case class ChoralesMetadata(parsedChorals: Int, firstSemiphraseLength: Double)

case class Model(
                  cf1Distribution: SelectionWheel,
                  cf2Transitions: FirstOrderTransitions,
                  initialSemiphraseTransitions: SecondOrderTransitions,
                  initialSemiphraseEndingChords: Map[ChordFigure, Double],
                  metadata: ChoralesMetadata
                  //                  middles: TransitionMatrix,
                  //                  last: TransitionMatrix
                ):

  def genFirstChord(r: Random): ChordFigure = cf1Distribution.find((prob, chord) => r.nextDouble() <= prob).getOrElse(cf1Distribution.last)._2

  def genSecondChord(cf1: ChordFigure)(r: Random): ChordFigure =
    val cf2Distribution = cf2Transitions(cf1)
    cf2Distribution.find((prob, chord) => r.nextDouble() <= prob).getOrElse(cf2Distribution.last)._2

  def endSemiphrase(currentLength: Int, currentChordEndingProbability: Double, r: Random): Boolean =
    val currentLengthBias = (currentLength / metadata.firstSemiphraseLength) * currentChordEndingProbability
    val random = r.nextDouble()
    println(s"($currentLength): chordProb = $currentChordEndingProbability - bias = $currentLengthBias - r = $random")
    if currentChordEndingProbability == 1.0 then return true
    if random < currentLengthBias then return true
    false

  def genInitialSemiphrase(r: Random): Vector[ChordFigure] =
    def rec(previousChords: (ChordFigure, ChordFigure), acc: Vector[ChordFigure], end: Boolean): Vector[ChordFigure] = end match
      case true => acc
      case false =>
        val nextChordDistribution: Try[SelectionWheel] = Try(initialSemiphraseTransitions(previousChords._1)(previousChords._2))
        nextChordDistribution match
          case Failure(e) =>
            println(s"Ending by failure: ${e.getMessage}")
            acc
          case Success(nCD) =>
            val nextChord = nCD.find((prob, chord) => r.nextDouble() <= prob).getOrElse(nCD.last)._2
            print(s"Ending with $nextChord ")
            val endNow = endSemiphrase(acc.size + 1, initialSemiphraseEndingChords.getOrElse(nextChord, 0.0), r)
            rec((previousChords._2, nextChord), acc :+ nextChord, endNow)

    println(s"First semiphrase length = ${metadata.firstSemiphraseLength}")
    val cf1 = genFirstChord(r)
    val cf2 = genSecondChord(cf1)(r)
    rec((cf1, cf2), Vector(cf1, cf2), false)

end Model
