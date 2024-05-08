package generator

import dsl.*
import scala.annotation.tailrec


def harmonizeChoral(choral: GeneratedChoral): HarmonizedChoral =
  HarmonizedChoral(choral.semiphrases.map(harmonizeSemiphrase), choral)

def harmonizeSemiphrase(semiphrase: Vector[Chord]): HarmonizedSemiphrase =
  val bassLine = generateBassLine(semiphrase.map(_.bass))
  generateHarmonizedSemiphrase(bassLine, semiphrase.map(chord => chord.bass +: chord.notes))

def generateBassLine(bassNotes: Vector[Note]): Vector[NoteWithOctave] =
  @tailrec
  def rec(notes: Vector[Note], acc: Vector[NoteWithOctave]): Vector[NoteWithOctave] = notes match
    case x +: xs => rec(xs, acc :+ acc.last.nearestNoteInRange(x, VoiceBounds.bass))
    case _ => acc

  @tailrec
  def review(tail: Vector[NoteWithOctave], acc: Vector[NoteWithOctave]): Vector[NoteWithOctave] = tail match
    case x1 +: x2 +: x3 +: xs if x1 equals x2 =>
      if x2.lowerOctave.isInRange(VoiceBounds.bass) then
        if math.abs(x2.lowerOctave.absolutPitch - x3.absolutPitch) > 12 then review(xs, acc :+ x1 :+ x2.lowerOctave :+ x3.lowerOctave)
        else review(xs, acc :+ x1 :+ x2.lowerOctave :+ x3)
      else review(tail.tail, acc :+ x1)
    case x1 +: x2 +: x3 +: xs => review(tail.tail, acc :+ x1)
    case x2 +: x3 +: Vector() if x2.absolutPitch > x3.absolutPitch =>
      if x2.lowerOctave.isInRange(VoiceBounds.bass) then
        println(s"$x2 lowed going to $x3")
        acc :+ x2.lowerOctave :+ x3
      else if x3.upperOctave.isInRange(VoiceBounds.bass) then
        println(s"$x3 upped arriving from $x2")
        acc :+ x2 :+ x3.upperOctave
      else
        println(s"None moved in $x2 to $x3")
        acc :+ x2 :+ x3
    case _ => acc ++ tail

  val bassLine = rec(bassNotes.tail, Vector(NoteWithOctave.lowestOctaveInRange(bassNotes.head, VoiceBounds.bass)))
  review(bassLine.tail, Vector(bassLine.head))

def generateHarmonizedSemiphrase(bassLine: Vector[NoteWithOctave], chords: Vector[Vector[Note]]): HarmonizedSemiphrase =
  @tailrec
  def rec(
           bassLine: Vector[NoteWithOctave],
           chords: Vector[Vector[Note]],
           lastChord: HarmonizedChord,
           acc: Vector[HarmonizedChord]): HarmonizedSemiphrase = (bassLine, chords) match
    case (b +: bs, cs +: css) =>
      val treble = lastChord.treble.nearestNoteInRangeFromChord(cs, VoiceBounds.treble)
      val contra = lastChord.contra.nearestNoteInRangeFromChord(cs, VoiceBounds.contra)
      val tenor = lastChord.tenor.nearestNoteInRangeFromChord(cs, VoiceBounds.tenor)
      val newChord = HarmonizedChord(b, tenor, contra, treble)
      rec(bs, css, newChord, acc :+ newChord)
    case _ => HarmonizedSemiphrase(acc)

  val firstChord = HarmonizedChord(
    bassLine.head,
    NoteWithOctave(chords.head(1), Octave._3),
    NoteWithOctave.lowestOctaveInRange(chords.head(2), VoiceBounds.contra),
    NoteWithOctave(chords.head(3), Octave._4)
  )
  rec(bassLine.tail, chords.tail, firstChord, firstChord +: Vector.empty)


