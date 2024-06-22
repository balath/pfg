import munit.FunSuite
import dsl.*
import dsl.Note.{b, *}
import dsl.ChordFigure.*
import dsl.Interval.*
import VoiceRange.*

import scala.language.postfixOps

class DSLTest extends FunSuite {
  //    Clue for one by one interval test
  //    val clue = s"Wrong ${interval.toString} interval for ${note.toString}: expected ${expected.toString} but obtained ${obtained.toString}"
  val majorModesNotes = Vector(
    Vector(c, d, e, f, g, a, b),
    Vector(cis, dis, eis, fis, gis, ais, bis),
    Vector(des, ees, f, ges, aes, bes, c),
    Vector(d, e, fis, g, a, b, cis),
    Vector(ees, f, g, aes, bes, c, d),
    Vector(e, fis, gis, a, b, cis, dis),
    Vector(f, g, a, bes, c, d, e),
    Vector(fis, gis, ais, b, cis, dis, eis),
    Vector(ges, aes, bes, ces, des, ees, f),
    Vector(g, a, b, c, d, e, fis),
    Vector(aes, bes, c, des, ees, f, g),
    Vector(a, b, cis, d, e, fis, gis),
    Vector(bes, c, d, ees, f, g, a),
    Vector(b, cis, dis, e, fis, gis, ais),
  )

  val minorModesNotes = for {
    mode <- majorModesNotes
    (a, b) = mode.splitAt(5)
  } yield b ++ a

  val e2 = NoteWithOctave(e, Octave._2)
  val dis2 = NoteWithOctave(dis, Octave._2)
  val c3 = NoteWithOctave(c, Octave._3)
  val f3 = NoteWithOctave(f, Octave._3)
  val g3 = NoteWithOctave(g, Octave._3)
  val b3 = NoteWithOctave(b, Octave._3)
  val c4 = NoteWithOctave(c, Octave._4)
  val g4 = NoteWithOctave(g, Octave._4)
  val c5 = NoteWithOctave(c, Octave._5)
  val cis4 = NoteWithOctave(cis, Octave._4)
  val d4 = NoteWithOctave(d, Octave._4)
  val d5 = NoteWithOctave(d, Octave._5)
  val fis4 = NoteWithOctave(fis, Octave._4)
  val fes2 = NoteWithOctave(fes, Octave._2)
  val a4 = NoteWithOctave(a, Octave._4)

  test("Perfect and majors intervals on major modes tonics should result on major mode notes"):
    val majorAndPerfectIntervals = Vector(Interval.maj2, Interval.maj3, Interval.perf5, Interval.maj7)
    val obtained = majorModesNotes.map(mode => (majorAndPerfectIntervals.map(mode.head.interval(_)), mode))
    assert(obtained.forall((results, mode) => results.forall(mode.contains(_))), obtained.toString)
  test("Minor, diminished and augmented intervals on major modes tonics should not result on major mode notes"):
    val minorDimAndAugIntervals = Vector(Interval.min2, Interval.min3, Interval.dis5, Interval.aug5, Interval.dis7, Interval.min7)
    val obtained = majorModesNotes.map(mode => (minorDimAndAugIntervals.map(mode.head.interval(_)), mode))
    assert(obtained.forall((results, mode) => results.forall(!mode.contains(_))), obtained.toString)
  test("Minor mode intervals on minor modes tonics should result on minor mode notes"):
    val minorModeIntervals = Vector(Interval.maj2, Interval.min3, Interval.perf5, Interval.min7)
    val obtained = minorModesNotes.map(mode => (minorModeIntervals.map(mode.head.interval(_)), mode))
    assert(obtained.forall((results, mode) => results.forall(mode.contains(_))), obtained.toString)
  test("Intervals over c and over g are properly obtained"):
    val cExpected: Vector[Note] = Vector(c, cis, deses, des, d, dis, eeses, ees, e, eis, fes, f, fis, ges, g, gis, aeses, aes, a, ais, beses, bes, b, bis, ces, c)
    val gExpected: Vector[Note] = Vector(g, gis, aeses, aes, a, ais, beses, bes, b, bis, ces, c, cis, des, d, dis, eeses, ees, e, eis, fes, f, fis, fisis, ges, g)
    val intervals: Vector[Interval] = Interval.values.toVector
    val cObtained: Vector[Note] = intervals map (c interval)
    val gObtained: Vector[Note] = intervals map (g interval)
    println(intervals.mkString(" - "))
    cObtained zip cExpected foreach :
      t => assertEquals(t._1, t._2)
    gObtained zip gExpected foreach :
      t => assertEquals(t._1, t._2)
  test("Chords are created properly"):
    val chords = Vector(iv_v, viidis43_v, v6, v, iisemdis65_v, V65_v, iisemdis6_v, V7_v, V, bVII)
    val expectedChords = Vector(
      Chord(iv_v, c, c, ees, g, None, Vector(c, ees, g),fis, Grade.iv,false),
      Chord(viidis43_v, c, fis, a, c, Some(ees), Vector(fis, a, c, ees), fis, Grade.vii, true),
      Chord(v6, bes, g, bes, d, None, Vector(g, bes, d), b, Grade.v, false),
      Chord(v, g, g, bes, d, None, Vector(g, bes, d), b, Grade.v, false),
      Chord(iisemdis65_v, c, a, c, ees, Some(g), Vector(a, c, ees, g), fis, Grade.ii, true),
      Chord(V65_v, fis, d, fis, a, Some(c), Vector(d, fis, a, c), fis, Grade.V, false),
      Chord(iisemdis6_v, c, a, c, ees, Some(g), Vector(a, c, ees, g), fis, Grade.ii, true),
      Chord(V7_v, d, d, fis, a, Some(c), Vector(d, fis, a, c), fis, Grade.V, false),
      Chord(V, g, g, b, d, None, Vector(g, b, d), b, Grade.V, false),
      Chord(bVII, bes, bes, d, f, None, Vector(bes, d, f), d, Grade.bVII, false)
    )
    chords map (c chord) zip expectedChords foreach :
      (obtained, expected) =>
        assert(
          obtained.equals(expected),
          s"At chord ${obtained.figure}, when expected $expected is obtained: $obtained"
        )
  test("Bass notes with octave are detected in its range or out of it"):
    assert(e2 isInRange bass)
    assert(c3 isInRange bass)
    assert(c4 isInRange bass)
    assert(!(dis2 isInRange bass))
    assert(!(cis4 isInRange bass))
  test("Inverse intervals are returned properly"):
    Interval.values foreach :
      interval => assertEquals(interval, interval.inverse.inverse)

    Interval.values map (i => ((i semitones) + ((i inverse) semitones), (i diatonic) + ((i inverse) diatonic))) foreach :
      tuple =>
        assertEquals(tuple._1, 12)
        assertEquals(tuple._2, 9)
  test("Disminished an augmented intervals are detected when present"):
    println(Interval.values.map(i => s"$i => ${i.isDisminishedOrAugmented}").mkString("\n"))
    true

  test("Notes of a chord are properly orderer by nearness to a pitch"):
    val pitch = NoteWithOctave(a, Octave._4).absolutPitch
    val notes = c.chord(V7_v).notes
    val expected = Vector(a4, fis4, c5, d5)
    val obtained = notes.orderByNearness(pitch, treble)
    assertEquals(obtained, expected)
  test("Harmonization restrictions are properly detected"):
    import generator._
    val unisonChord = HarmonizedChord(e2, c4, c4, d5)
    val crossedChord = HarmonizedChord(e2,d4,c4,d5)
    val wrongDistanceChord = HarmonizedChord(e2, f3, c4, d5)
    val validChord = HarmonizedChord(e2,c4,d4,d5)

    assert(unisons(unisonChord))
    assert(!unisons(validChord))
    assert(crossedVoices(crossedChord))
    assert(!crossedVoices(validChord))
    assert(wrongVoicesDistance(wrongDistanceChord))
    assert(!wrongVoicesDistance(validChord))
  test("Intervals are identified correctly from two notes"):
    assertEquals(c getInterval g, perf5)
    assertEquals(e getInterval c, min6)
    assertEquals(g getInterval g, unis)
    assertEquals(g getInterval fis, maj7)
    assertEquals(g getInterval f, min7)
    assertEquals(g getInterval fes, dis7)

  test("Melodic intervals are identified correctly from two notes with octave"):
    assertEquals(c3 getMelodicInterval e2, min6)
    assertEquals(e2 getMelodicInterval c5, min6)
    assertEquals(c4 getMelodicInterval c4, unis)
    assertEquals(c4 getMelodicInterval c5, perf8)
    assertEquals(g3 getMelodicInterval fis4, maj7)
    assertEquals(g3 getMelodicInterval f3, maj2)
    assertEquals(g3 getMelodicInterval fes2, aug2)

  test("notes in a chord are proprely ordered by nearness"):
    val chord = c.chord(ChordFigure.V65)
    val obtained = chord.notes.orderByNearness(NoteWithOctave(aes,Octave._3).absolutPitch,tenor)
    val expected = Vector(g3, b3, f3, d4)
    assertEquals(obtained, expected)

}