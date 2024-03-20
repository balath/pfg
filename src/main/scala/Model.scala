package model

import cats.kernel.Previous
import dsl.{ChordFigure, Mode, Note}

import scala.util.Random

type ChainElement = (Double, ChordFigure)
type Distribution = Vector[ChainElement]
type FirstOrderChain = Map[ChordFigure, Distribution]
type SecondOrderChain = Map[ChordFigure, Map[ChordFigure, Distribution]]

case class ChoralsMetadata(parsedChorals: Int, keyAndModeMap: Map[(Note, Mode), Int])

case class Model(
                  cf1Distribution: Distribution,
                  cf2Transitions: FirstOrderChain,
                  initialSemiphraseTransitions: SecondOrderChain
                  //                  middles: TransitionMatrix,
                  //                  last: TransitionMatrix
                ):

  def genFirstChord(r: Random): ChordFigure = cf1Distribution.find((prob, chord) => r.nextDouble() <= prob).getOrElse(cf1Distribution.last)._2

  def genSecondChord(cf1: ChordFigure)(r: Random): ChordFigure =
    val cf2Distribution = cf2Transitions(cf1)
    cf2Distribution.find((prob, chord) => r.nextDouble() <= prob).getOrElse(cf2Distribution.last)._2

  def genInitialSemiphrase(r: Random): Vector[ChordFigure] =
    def rec(previousChords: (ChordFigure, ChordFigure), acc: Vector[ChordFigure],end: Int): Vector[ChordFigure] = end match
      case 0 => acc
      case _ => 
        val nextChordDistribution = initialSemiphraseTransitions(previousChords._1)(previousChords._2)
        val nextChord = nextChordDistribution.find((prob, chord) => r.nextDouble() <= prob).getOrElse(nextChordDistribution.last)._2
        rec((previousChords._2, nextChord), acc :+ nextChord, end - 1)
    
    val cf1 = genFirstChord(r)
    val cf2 = genSecondChord(cf1)(r)
    rec((cf1, cf2), Vector(cf1,cf2),10)



end Model
