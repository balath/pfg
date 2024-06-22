package dsl

import common.DataRegex.*

import scala.annotation.tailrec
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

type Semiphrase = Vector[ChordFigure]
type GeneratedSemiphrase = Vector[Chord]

case class HarmonizedChord(bass: NoteWithOctave, tenor: NoteWithOctave, contra: NoteWithOctave, treble: NoteWithOctave):
  override def toString: String = s"<$bass $tenor $contra $treble>"

case class HarmonizedSemiphrase(semiphrase: Vector[HarmonizedChord]):
  def voiceToString(voice: Vector[NoteWithOctave], midiFermataCode: Boolean, finalDuration: String): String =
    val start = s"\t${voice.head}4"
    val body = voice.tail.dropRight(1).mkString(" ")
    val end =
      if midiFermataCode then s"${voice.last}2 r4\n"
      else s"${voice.last}$finalDuration\\fermata \n"
    s"$start $body $end"

  def bassToString(midiFermataCode: Boolean, finalDuration: String = "4"): String =
    voiceToString(semiphrase.map(_.bass), midiFermataCode, finalDuration)

  def tenorToString(midiFermataCode: Boolean, finalDuration: String = "4"): String =
    voiceToString(semiphrase.map(_.tenor), midiFermataCode, finalDuration)

  def contraToString(midiFermataCode: Boolean, finalDuration: String = "4"): String =
    voiceToString(semiphrase.map(_.contra), midiFermataCode, finalDuration)

  def trebleToString(midiFermataCode: Boolean, finalDuration: String = "4"): String =
    voiceToString(semiphrase.map(_.treble), midiFermataCode, finalDuration)

case class HarmonizedChoral(semiphrases: Vector[HarmonizedSemiphrase], choralHarmony: GeneratedChoral):
  def toLilypondFileFormat(midiFermataCode: Boolean): String =
    val semiphrasesButLast = semiphrases.dropRight(1)
    val lastSemiphrase = semiphrases.last
    val finalDuration = semiphrases.map(_.semiphrase.length).sum % 4 match
      case 1 => "1"
      case 2 => "2."
      case 3 => "2"
      case _ => "4"
    val bassLine = s"${
      semiphrasesButLast.map(_.bassToString(midiFermataCode)).mkString("\n\t")
    }\n\t${
      lastSemiphrase.bassToString(midiFermataCode, finalDuration)
    }"
    val tenorLine = s"${
      semiphrasesButLast.map(_.tenorToString(midiFermataCode)).mkString("\n\t")
    }\n\t${
      lastSemiphrase.tenorToString(midiFermataCode, finalDuration)
    }"
    val contraLine = s"${
      semiphrasesButLast.map(_.contraToString(midiFermataCode)).mkString("\n\t")
    }\n\t${
      lastSemiphrase.contraToString(midiFermataCode, finalDuration)
    }"
    val trebleLine = s"${
      semiphrasesButLast.map(_.trebleToString(midiFermataCode)).mkString("\n\t")
    }\n\t${
      lastSemiphrase.trebleToString(midiFermataCode, finalDuration)
    }"

    val lyrics: String = choralHarmony.semiphrases
      .map(_.map(c => s"\"${
        c.figure.toString
          .replace("semdis", "ø")
          .replace("dis", "o")
          .replace("aug", "+")
          .replace("_", "/")
      }\""
      ).mkString(" ")).mkString(" ")
    val midiCode = if midiFermataCode then "  \\midi {\n    \\tempo 4 = 60 \n  } " else ""
    val music =
      s"""
         |\\version \"2.18.2\"
         |\\score {
         |    <<
         |         \\new Staff << \\clef \"treble\" \\key ${choralHarmony.key} \\${choralHarmony.mode}
         |            \\new Voice = \"treble\" { \\set Staff.midiInstrument = #"choir aahs" \\voiceOne $trebleLine }
         |            \\new Voice = \"contra\" { \\set Staff.midiInstrument = #"choir aahs" \\voiceTwo $contraLine }
         |         >>
         |         \\new Staff << \\clef \"bass\" \\key ${choralHarmony.key} \\${choralHarmony.mode}
         |            \\new Voice = \"tenor\" { \\set Staff.midiInstrument = #"choir aahs" \\voiceOne $tenorLine }
         |            \\new Voice = \"bass\" { \\set Staff.midiInstrument = #"choir aahs" \\voiceTwo $bassLine }
         |         \\new Lyrics \\lyricsto "bass" { $lyrics }
         |         >>
         |    >>
         |  \\layout {}
         |$midiCode
         |}
      """.stripMargin
    music

case class Choral(num: Int, key: Note, mode: Mode, semiphrases: Vector[Semiphrase])

enum VoiceRange(val lowerBound: NoteWithOctave, val upperBound: NoteWithOctave, val middleNote: NoteWithOctave):
  case bass extends VoiceRange(NoteWithOctave(Note.e, Octave._2), NoteWithOctave(Note.c, Octave._4), NoteWithOctave(Note.d, Octave._3))
  case tenor extends VoiceRange(NoteWithOctave(Note.e, Octave._3), NoteWithOctave(Note.g, Octave._4), NoteWithOctave(Note.b, Octave._3))
  case contra extends VoiceRange(NoteWithOctave(Note.g, Octave._3), NoteWithOctave(Note.d, Octave._5), NoteWithOctave(Note.e, Octave._4))
  case treble extends VoiceRange(NoteWithOctave(Note.d, Octave._4), NoteWithOctave(Note.g, Octave._5), NoteWithOctave(Note.a, Octave._4))

  def absoluteUpper: Int = upperBound.note.pitch + (12 * (upperBound.octave.octave - 2))

  def absoluteLower: Int = lowerBound.note.pitch + (12 * (lowerBound.octave.octave - 2))

case class NoteWithOctave(note: Note, octave: Octave):
  infix def -(that: NoteWithOctave): Int = this.absolutPitch - that.absolutPitch

  def absolutPitch: Int = this.note match {
    case Note.ces | Note.ceses => note.pitch + (12 * (octave.octave - 3) )
    case Note.bis | Note.bisis => note.pitch + (12 * (octave.octave - 1) )
    case _ => note.pitch + (12 * (octave.octave - 2) )
  }

  def isInRange(bounds: VoiceRange): Boolean =
    val upper = absolutPitch <= bounds.absoluteUpper
    val lower = absolutPitch >= bounds.absoluteLower
    upper && lower

  def nearestNoteInRange(note: Note, voiceBounds: VoiceRange): NoteWithOctave =
    Vector(Octave._2, Octave._3, Octave._4, Octave._5)
      .map(NoteWithOctave(note, _))
      .filter(_.isInRange(voiceBounds))
      .minBy(nwo => math.abs(this.absolutPitch - nwo.absolutPitch))

  def nearestNoteInRangeFromChord(chord: Chord, voiceBounds: VoiceRange): NoteWithOctave =
    Vector(Octave._2, Octave._3, Octave._4, Octave._5)
      .flatMap(octave => chord.notes.map(NoteWithOctave(_, octave)))
      .filter(_.isInRange(voiceBounds))
      .distinct
      .minBy(nwo => math.abs(this.absolutPitch - nwo.absolutPitch))

  def lowerOctave: NoteWithOctave = NoteWithOctave(note, octave.lowerOctave)

  def upperOctave: NoteWithOctave = NoteWithOctave(note, octave.upperOctave)

  def getMelodicInterval(that: NoteWithOctave): Interval =
    val (lower, upper) = if this.absolutPitch - that.absolutPitch > 0 then (that, this) else (this, that)
    if (lower.note.getInterval(upper.note) equals Interval.unis) && (lower.octave - upper.octave != 0) then Interval.perf8
    else lower.note.getInterval(upper.note)

  override def toString: String = s"$note${octave.lilypondCode}"

  override def equals(obj: Any): Boolean = obj match
    case that: NoteWithOctave => this.note.equals(that.note) && this.octave.equals(that.octave)
    case _ => false
end NoteWithOctave

case object NoteWithOctave:
  def nearestNoteInRangeFromChord(pitch: Int, notes: Vector[Note], voiceBounds: VoiceRange): NoteWithOctave =
    Vector(Octave._2, Octave._3, Octave._4, Octave._5)
      .flatMap(octave => notes.map(NoteWithOctave(_, octave)))
      .filter(_.isInRange(voiceBounds))
      .minBy(nwo => math.abs(pitch - nwo.absolutPitch))

  def lowestOctaveInRange(note: Note, voiceBounds: VoiceRange): NoteWithOctave =
    Octave.values.map(octave => NoteWithOctave(note, octave)).filter(_.isInRange(voiceBounds)).minBy(_.absolutPitch)

case class GeneratedChoral(key: Note, mode: Mode, semiphrases: Vector[Vector[Chord]])
case class Chord(figure: ChordFigure, bass: Note, tonic: Note, third: Note, fifth: Note, seventh: Option[Note], notes: Vector[Note], leadingNote: Note, grade: Grade, isDisminished: Boolean):
  override def toString: String = s"<$bass ${notes.mkString(" ")}>"
extension (notes: Vector[Note])
  infix def -(note: Note): Vector[Note] = notes.filter(!_.equals(note))
  infix def orderByNearness(pitch: Int, range: VoiceRange): Vector[NoteWithOctave] =
    @tailrec
    def rec(tail: Vector[Note], acc: Vector[NoteWithOctave]): Vector[NoteWithOctave] = tail match
      case Vector() => acc
      case _ =>
        val nearestNote = NoteWithOctave.nearestNoteInRangeFromChord(pitch, tail, range)
        rec(tail - nearestNote.note, acc :+ nearestNote)

    rec(notes, Vector())

enum Mode:
  case maj extends Mode
  case min extends Mode

  override def toString: String = if this equals maj then "major" else "minor"
end Mode

enum Octave(val octave: Int, val lilypondCode: String):
  case _2 extends Octave(2, ",")
  case _3 extends Octave(3, "")
  case _4 extends Octave(4, "'")
  case _5 extends Octave(5, "''")

  infix def -(that: Octave): Int = this.octave - that.octave

  def upperOctave: Octave = this match
    case Octave._2 => Octave._3
    case Octave._3 => Octave._4
    case Octave._4 => Octave._5
    case Octave._5 => Octave._5

  def lowerOctave: Octave = this match
    case Octave._2 => Octave._2
    case Octave._3 => Octave._2
    case Octave._4 => Octave._3
    case Octave._5 => Octave._4

end Octave

case object Octave:
  def apply(n: Int): Octave = n match
    case n if n <= 2 => _2
    case 3 => _3
    case 4 => _4
    case n if n >= 5 => _5

/**
 * Musical notes are named according to the LylyPond standard for english (https://lilypond.org/):
 *  - "a" to "g" for pitch
 *  - "s" for sharp, "f" for flat, "x" for double sharp and "ff" for double flat
 */
enum Note(val pitch: Int):
  case bis extends Note(0)
  case c extends Note(0)
  case deses extends Note(0)
  case bisis extends Note(1)
  case cis extends Note(1)
  case des extends Note(1)
  case cisis extends Note(2)
  case d extends Note(2)
  case eeses extends Note(2)
  case dis extends Note(3)
  case ees extends Note(3)
  case feses extends Note(3)
  case disis extends Note(4)
  case e extends Note(4)
  case fes extends Note(4)
  case eis extends Note(5)
  case f extends Note(5)
  case geses extends Note(5)
  case eisis extends Note(6)
  case fis extends Note(6)
  case ges extends Note(6)
  case fisis extends Note(7)
  case g extends Note(7)
  case aeses extends Note(7)
  case gis extends Note(8)
  case aes extends Note(8)
  case gisis extends Note(9)
  case a extends Note(9)
  case beses extends Note(9)
  case ais extends Note(10)
  case bes extends Note(10)
  case ceses extends Note(10)
  case aisis extends Note(11)
  case b extends Note(11)
  case ces extends Note(11)

  val diatonicScaleNumNote: Map[Int, String] = Map(0 -> "a", 1 -> "b", 2 -> "c", 3 -> "d", 4 -> "e", 5 -> "f", 6 -> "g")
  val diatonicScaleNoteNum: Map[String, Int] = Map("a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4, "f" -> 5, "g" -> 6)

  def interval(interval: Interval): Note =
    val newPitch = (this.pitch + interval.semitones) % 12
    val newNoteKey = (diatonicScaleNoteNum(this.toString.substring(0, 1)) + interval.diatonic - 1) % 7
    val newNoteName: String = diatonicScaleNumNote(newNoteKey)
    val newPitchNotes = Note.values.filter(_.pitch == newPitch)
    newPitchNotes.find(_.toString.startsWith(newNoteName)).getOrElse(newPitchNotes.head)

  def getInterval(that: Note): Interval =
    val diatonicInterval = (
      (diatonicScaleNoteNum(that.toString.substring(0, 1)) + 7) - diatonicScaleNoteNum(this.toString.substring(0, 1))
      ) % 7 + 1
    val chromaticInterval = ((that.pitch + 12) - this.pitch) % 12
    val interval = Try(Interval.values.toVector.filter(i => i.diatonic == diatonicInterval && i.semitones == chromaticInterval).head)
    if interval.isSuccess then interval.get
    else throw new NoSuchElementException(s"Interval not found for $this to $that (diatonic = $diatonicInterval, chromatic = $chromaticInterval")

  def chord(chordFigure: ChordFigure): Chord =
    import Interval._, Mode._
    val (grade, kind, inversion, base) = chordFigure.toString match
      case chordRegex(g, k, i, b) => (Grade.valueOf(g), Try(Kind.valueOf(k)), Try(i.toInt), Try(Grade.valueOf(b)))
    //    println(s"Grade = $grade, Kind = $kind, Inversion = $inversion, Base = $base")
    val tonic = if base.isSuccess then this.interval(base.get.interval).interval(grade.interval) else this.interval(grade.interval)
    val leadingNote: Note = (base, grade) match
      case (Success(b), g) if g.isMajorRelativeGrade => this.interval(b.interval).interval(maj2)
      case (Success(b), g) => this.interval(b.interval).interval(maj7)
      case (Failure(_), g) if g.isMajorRelativeGrade => this.interval(maj2)
      case (Failure(_), g) => this.interval(maj7)

    val third = if grade.mode.equals(min) then tonic interval min3 else tonic interval maj3
    val fifth = kind match
      case Failure(_) => tonic interval perf5
      case Success(Kind.semdis) | Success(Kind.dis) => tonic interval dis5
      case Success(Kind.aug) => tonic interval aug5
    val seventh: Option[Note] = if kind.getOrElse(Kind.dis).equals(Kind.semdis) then Some(tonic interval min7)
    else if inversion.isFailure then None
    else inversion.get match
      case 6 | 64 => None
      case 7 | 65 | 43 | 42 =>
        if kind.getOrElse(Kind.semdis).equals(Kind.dis) then Some(tonic interval dis7)
        else if grade.mode.equals(min) || grade.equals(Grade.V) || grade.equals(Grade.bVII) then Some(tonic interval min7)
        else Some(tonic interval maj7)
    val notes = if seventh isDefined then Vector(tonic, third, fifth, seventh.get) else Vector(tonic, third, fifth)
    val disminished = kind.isSuccess && !kind.get.equals(Kind.aug)
    inversion match
      case Failure(_) | Success(7) => Chord(chordFigure, tonic, tonic, third, fifth, seventh, notes, leadingNote, grade, disminished)
      case Success(6) | Success(65) => Chord(chordFigure, third, tonic, third, fifth, seventh, notes, leadingNote, grade, disminished)
      case Success(64) | Success(43) => Chord(chordFigure, fifth, tonic, third, fifth, seventh, notes, leadingNote, grade, disminished)
      case Success(42) => Chord(chordFigure, seventh.get, tonic, third, fifth, seventh, notes, leadingNote, grade, disminished)

  def inOctave(n: Int): NoteWithOctave = NoteWithOctave(this, Octave(n))

  def grade(grade: Grade): Note = this interval grade.interval

end Note

enum Kind:
  case semdis extends Kind
  case dis extends Kind
  case aug extends Kind

enum Grade(val interval: Interval, val mode: Mode):
  case I extends Grade(Interval.unis, Mode.maj)
  case i extends Grade(Interval.unis, Mode.min)
  case bII extends Grade(Interval.min2, Mode.maj)
  case II extends Grade(Interval.maj2, Mode.maj)
  case ii extends Grade(Interval.maj2, Mode.min)
  case bIII extends Grade(Interval.min3, Mode.maj)
  case iii extends Grade(Interval.maj3, Mode.min)
  case IV extends Grade(Interval.perf4, Mode.maj)
  case iv extends Grade(Interval.perf4, Mode.min)
  case V extends Grade(Interval.perf5, Mode.maj)
  case v extends Grade(Interval.perf5, Mode.min)
  case bVI extends Grade(Interval.min6, Mode.maj)
  case vi extends Grade(Interval.maj6, Mode.min)
  case bVII extends Grade(Interval.min7, Mode.maj)
  case bvii extends Grade(Interval.min7, Mode.min)
  case vii extends Grade(Interval.maj7, Mode.min)

  def isDominant: Boolean = this match
    case Grade.V | Grade.vii | Grade.bVII => true
    case _ => false

  def isMajorRelativeGrade: Boolean = this match
    case Grade.bVII | Grade.bVI | Grade.bIII => true
    case _ => false


end Grade

enum Interval(val diatonic: Int, val semitones: Int):
  case unis extends Interval(1, 0)
  case aug1 extends Interval(1, 1)
  case dis2 extends Interval(2, 0)
  case min2 extends Interval(2, 1)
  case maj2 extends Interval(2, 2)
  case aug2 extends Interval(2, 3)
  case dis3 extends Interval(3, 2)
  case min3 extends Interval(3, 3)
  case maj3 extends Interval(3, 4)
  case aug3 extends Interval(3, 5)
  case dis4 extends Interval(4, 4)
  case perf4 extends Interval(4, 5)
  case aug4 extends Interval(4, 6)
  case dis5 extends Interval(5, 6)
  case perf5 extends Interval(5, 7)
  case aug5 extends Interval(5, 8)
  case dis6 extends Interval(6, 7)
  case min6 extends Interval(6, 8)
  case maj6 extends Interval(6, 9)
  case aug6 extends Interval(6, 10)
  case dis7 extends Interval(7, 9)
  case min7 extends Interval(7, 10)
  case maj7 extends Interval(7, 11)
  case aug7 extends Interval(7, 12)
  case dis8 extends Interval(8, 11)
  case perf8 extends Interval(8, 12)

  def isFifthOrEighth: Boolean = this.diatonic == 5 || this.diatonic == 8

  def isDisminishedOrAugmented: Boolean = this match {
    case Interval.aug2 | Interval.dis3 | Interval.aug3 | Interval.dis4 | Interval.aug4 | Interval.dis5 | Interval.aug5 | Interval.dis6 | Interval.aug6 | Interval.dis7 | Interval.aug7 | Interval.dis8 => true
    case _ => false
  }

  def inverse: Interval = this match
    case Interval.unis => perf8
    case Interval.aug1 => dis8
    case Interval.dis2 => aug7
    case Interval.min2 => maj7
    case Interval.maj2 => min7
    case Interval.aug2 => dis7
    case Interval.dis3 => aug6
    case Interval.min3 => maj6
    case Interval.maj3 => min6
    case Interval.aug3 => dis6
    case Interval.dis4 => aug5
    case Interval.perf4 => perf5
    case Interval.aug4 => dis5
    case Interval.dis5 => aug4
    case Interval.perf5 => perf4
    case Interval.aug5 => dis4
    case Interval.dis6 => aug3
    case Interval.min6 => maj3
    case Interval.maj6 => min3
    case Interval.aug6 => dis3
    case Interval.dis7 => aug2
    case Interval.min7 => maj2
    case Interval.maj7 => min2
    case Interval.aug7 => dis2
    case Interval.dis8 => aug1
    case Interval.perf8 => unis

end Interval

object Interval:
  def unapply(interval: Interval): Option[(Int, Int)] =
    Some((interval.diatonic, interval.semitones))

enum IntervalDirection:
  case asc extends IntervalDirection
  case desc extends IntervalDirection
  case hold extends IntervalDirection

  def isCounterMovementTo(that: IntervalDirection): Boolean = !this.equals(that)

case object IntervalDirection:
  def apply(from: NoteWithOctave, to: NoteWithOctave): IntervalDirection = to - from match
    case 0 => IntervalDirection.hold
    case n if n > 0 => IntervalDirection.asc
    case n if n < 0 => IntervalDirection.desc

enum ChordFigure:
  case Empty extends ChordFigure
  case I extends ChordFigure
  case V6 extends ChordFigure
  case IV6 extends ChordFigure
  case IV_IV extends ChordFigure
  case V_IV extends ChordFigure
  case IV extends ChordFigure
  case V extends ChordFigure
  case vi extends ChordFigure
  case viisemdis extends ChordFigure
  case I6 extends ChordFigure
  case I64 extends ChordFigure
  case V7 extends ChordFigure
  case V6_vi extends ChordFigure
  case V_V extends ChordFigure
  case viidis6_V extends ChordFigure
  case V_vi extends ChordFigure
  case vi6 extends ChordFigure
  case ii65 extends ChordFigure
  case ii7 extends ChordFigure
  case ii extends ChordFigure
  case iisemdis65_vi extends ChordFigure
  case V7_vi extends ChordFigure
  case iii6 extends ChordFigure
  case vi64 extends ChordFigure
  case V6_V extends ChordFigure
  case vi7 extends ChordFigure
  case ii6 extends ChordFigure
  case viidis6 extends ChordFigure
  case I7 extends ChordFigure
  case viisemdis42 extends ChordFigure
  case viisemdis65 extends ChordFigure
  case V7_VI extends ChordFigure
  case VI extends ChordFigure
  case V7_V extends ChordFigure
  case V7_IV extends ChordFigure
  case ii65_V extends ChordFigure
  case V43_V extends ChordFigure
  case V65_V extends ChordFigure
  case viidis_VI extends ChordFigure
  case V42 extends ChordFigure
  case visemdis43 extends ChordFigure
  case viidis43_vi extends ChordFigure
  case V65_vi extends ChordFigure
  case V7_bVII extends ChordFigure
  case bVII extends ChordFigure
  case V65 extends ChordFigure
  case V65_ii extends ChordFigure
  case V65_iii extends ChordFigure
  case iii extends ChordFigure
  case viidis7_vi extends ChordFigure
  case V65_v extends ChordFigure
  case v extends ChordFigure
  case V65_iv extends ChordFigure
  case iv extends ChordFigure
  case iv6 extends ChordFigure
  case V_iv extends ChordFigure
  case viidis6_iv extends ChordFigure
  case V43_iv extends ChordFigure
  case V6_iv extends ChordFigure
  case viidis65_iv extends ChordFigure
  case bVI_iv extends ChordFigure
  case iv7 extends ChordFigure
  case bIIIaug6_iv extends ChordFigure
  case iv64 extends ChordFigure
  case bVII6 extends ChordFigure
  case bIIIaug6 extends ChordFigure
  case i extends ChordFigure
  case i6 extends ChordFigure
  case iidis64 extends ChordFigure
  case v_iv extends ChordFigure
  case iv6_iv extends ChordFigure
  case viidis7_bVII extends ChordFigure
  case viidis7 extends ChordFigure
  case bIII extends ChordFigure
  case V43 extends ChordFigure
  case bVI extends ChordFigure
  case i64 extends ChordFigure
  case V6_v extends ChordFigure
  case V_v extends ChordFigure
  case viisemdis43_bVII extends ChordFigure
  case viidis6_bVII extends ChordFigure
  case iidis6 extends ChordFigure
  case viidis7_V extends ChordFigure
  case iiidis6 extends ChordFigure
  case bII_IV extends ChordFigure
  case viidis65 extends ChordFigure
  case V7_iv extends ChordFigure
  case IV_bVI extends ChordFigure
  case bVI6 extends ChordFigure
  case V_bVI extends ChordFigure
  case iv42 extends ChordFigure
  case iv_iv extends ChordFigure
  case V64 extends ChordFigure
  case viidis6_ii extends ChordFigure
  case V7_ii extends ChordFigure
  case IV7 extends ChordFigure
  case bVI7 extends ChordFigure
  case bIII6 extends ChordFigure
  case V7_bIII extends ChordFigure
  case iv65 extends ChordFigure
  case iisemdis65 extends ChordFigure
  case v6 extends ChordFigure
  case V42_bVII extends ChordFigure
  case IV65 extends ChordFigure
  case iisemdis65_iv extends ChordFigure
  case bVII7 extends ChordFigure
  case bVI64 extends ChordFigure
  case bVII7_ii extends ChordFigure
  case iv6_ii extends ChordFigure
  case bVII_vi extends ChordFigure
  case V42_ii extends ChordFigure
  case iv64_vi extends ChordFigure
  case viidis6_vi extends ChordFigure
  case V42_IV extends ChordFigure
  case iv_ii extends ChordFigure
  case bVI6_ii extends ChordFigure
  case iv65_vi extends ChordFigure
  case IV65_vi extends ChordFigure
  case ii64 extends ChordFigure
  case V_ii extends ChordFigure
  case visemdis extends ChordFigure
  case viidis extends ChordFigure
  case V65_bVII extends ChordFigure
  case bVI42 extends ChordFigure
  case bIII7 extends ChordFigure
  case bIII42 extends ChordFigure
  case iv_v extends ChordFigure
  case V7_v extends ChordFigure
  case IV65_v extends ChordFigure
  case bVII42 extends ChordFigure
  case bIII64 extends ChordFigure
  case iisemdis43 extends ChordFigure
  case V43_bVII extends ChordFigure
  case v7 extends ChordFigure
  case bVI6_v extends ChordFigure
  case bIII7_v extends ChordFigure
  case IV6_v extends ChordFigure
  case bIIIaug_iv extends ChordFigure
  case IV42 extends ChordFigure
  case IV_V extends ChordFigure
  case ii7_V extends ChordFigure
  case iv7_V extends ChordFigure
  case bIIIaug_V extends ChordFigure
  case ii42 extends ChordFigure
  case I65 extends ChordFigure
  case viidis65_ii extends ChordFigure
  case V6_ii extends ChordFigure
  case ii6_V extends ChordFigure
  case V42_V extends ChordFigure
  case IV65_V extends ChordFigure
  case viidis43_v extends ChordFigure
  case viidis7_v extends ChordFigure
  case iisemdis65_v extends ChordFigure
  case i42 extends ChordFigure
  case viidis42 extends ChordFigure
  case V65_IV extends ChordFigure
  case ii_V extends ChordFigure
  case bIIIaug6_ii extends ChordFigure
  case iisemdis42_v extends ChordFigure
  case IV6_vi extends ChordFigure
  case viidis43 extends ChordFigure
  case IV_bIII extends ChordFigure
  case iisemdis43_bIII extends ChordFigure
  case V6_bVII extends ChordFigure
  case ii65_bIII extends ChordFigure
  case bVII64 extends ChordFigure
  case V_bVII extends ChordFigure
  case v43 extends ChordFigure
  case vi65 extends ChordFigure
  case vidis6 extends ChordFigure
  case iii7 extends ChordFigure
  case vii6 extends ChordFigure
  case iisemdis42 extends ChordFigure
  case viidis7_ii extends ChordFigure
  case bVII65 extends ChordFigure
  case viidis_bVII extends ChordFigure
  case V7_bVI extends ChordFigure
  case iv7_vii extends ChordFigure
  case V7_vii extends ChordFigure
  case vii extends ChordFigure
  case V64_bVII extends ChordFigure
  case vi_bVII extends ChordFigure
  case iisemdis6_v extends ChordFigure
  case bIIIaug65 extends ChordFigure
  case iv7_v extends ChordFigure
  case V42_v extends ChordFigure
  case iisemdis extends ChordFigure
  case ii_v extends ChordFigure
  case viisemdis_bVII extends ChordFigure
  case viidis6_v extends ChordFigure
  case v64 extends ChordFigure
  case V64_v extends ChordFigure
  case bIIIaug65_iv extends ChordFigure
  case V64_iv extends ChordFigure
  case iv43 extends ChordFigure
  case bIIIaug64 extends ChordFigure
  case viisemdis43_iv extends ChordFigure
  case iidis6_iv extends ChordFigure
  case bVII_iv extends ChordFigure
  case bIII_iv extends ChordFigure
  case bIIIaug7_iv extends ChordFigure
  case iv65_iv extends ChordFigure
  case ii6_iv extends ChordFigure
  case iv7_iv extends ChordFigure
  case viisemdis_IV extends ChordFigure
  case ii_IV extends ChordFigure
  case vi_IV extends ChordFigure
  case ii65_IV extends ChordFigure
  case bVI7_ii extends ChordFigure
  case bIIIaug_ii extends ChordFigure
  case IV7_ii extends ChordFigure
  case iv6_vi extends ChordFigure
  case iisemdis43_vi extends ChordFigure
  case viidis6_bVI extends ChordFigure
  case viidis42_iv extends ChordFigure
  case V66_iv extends ChordFigure
  case viidis6_bIII extends ChordFigure
  case iisemdis42_iv extends ChordFigure
  case viidis7_iv extends ChordFigure
  case bIII43 extends ChordFigure
  case bIIIaug43_iv extends ChordFigure
  case bVI_bVII extends ChordFigure
  case bVII7_bVII extends ChordFigure
  case vii64 extends ChordFigure
  case v_bvii extends ChordFigure
  case bvii6 extends ChordFigure
  case viidis7_bvii extends ChordFigure
  case bvii extends ChordFigure
  case vi_bVI extends ChordFigure
  case bVI_bvii extends ChordFigure
  case bVII7_bvii extends ChordFigure
  case V65_bvii extends ChordFigure
  case iidis6_v extends ChordFigure
  case bIIIaug6_v extends ChordFigure
  case ii7_IV extends ChordFigure
  case IV6_bVII extends ChordFigure
  case ii65_bVII extends ChordFigure
  case viisemdis43_IV extends ChordFigure
  case viidis6_IV extends ChordFigure
  case viidis64_IV extends ChordFigure
  case iisemdis42_ii extends ChordFigure
  case bIIIaug42 extends ChordFigure
  case V65_II extends ChordFigure
  case visemdis42_v extends ChordFigure
  case iisemdis_II extends ChordFigure
  case V7_II extends ChordFigure
  case II extends ChordFigure
  case iisemdis43_v extends ChordFigure
  case ii7_ii extends ChordFigure
  case viidis_v extends ChordFigure
  case visemdis65_v extends ChordFigure
  case vi_V extends ChordFigure
  case IV7_V extends ChordFigure
  case visemdis_vi extends ChordFigure
  case iisemdis42_vi extends ChordFigure
  case I42 extends ChordFigure
  case iii64 extends ChordFigure
  case viisemdis_V extends ChordFigure
  case V42_vi extends ChordFigure
  case V43_ii extends ChordFigure
  case viisemdis42_V extends ChordFigure
  case vidis_v extends ChordFigure
  case viisemdis43 extends ChordFigure
  case ii43 extends ChordFigure
  case viidis64 extends ChordFigure
  case iidis6_vi extends ChordFigure
  case viidis7_iii extends ChordFigure
  case i7 extends ChordFigure
  case i43 extends ChordFigure
  case iidis extends ChordFigure
  case viisemdis42_bVI extends ChordFigure
  case vi42 extends ChordFigure
  case I43 extends ChordFigure
  case iii43_V extends ChordFigure
  case viisemdis65_V extends ChordFigure
  case bIII_ii extends ChordFigure
  case bVII_ii extends ChordFigure
  case iidis6_ii extends ChordFigure
  case viisemdis_v extends ChordFigure
  case viisemdis_bVI extends ChordFigure
  case V43_vi extends ChordFigure
  case bVI_ii extends ChordFigure
  case iisemdis_vi extends ChordFigure
  case IV43 extends ChordFigure
  case I42_V extends ChordFigure
  case vi7_V extends ChordFigure
  case ii64_V extends ChordFigure
  case iii6_V extends ChordFigure
  case IV6_V extends ChordFigure
  case iisemdis65_ii extends ChordFigure
  case vi43 extends ChordFigure
  case v64_ii extends ChordFigure
  case IV6_IV extends ChordFigure
  case ii43_IV extends ChordFigure
  case V6_IV extends ChordFigure
  case IV7_IV extends ChordFigure
  case iisemdis65_V extends ChordFigure
  case viisemdis_ii extends ChordFigure
  case v_ii extends ChordFigure
  case V64_V extends ChordFigure
  case V6_iii extends ChordFigure
  case viidis_ii extends ChordFigure
  case V_iii extends ChordFigure
  case vidis64_V extends ChordFigure
  case IV_bVII extends ChordFigure
  case iidis7 extends ChordFigure
  case ii6_bVI extends ChordFigure

end ChordFigure