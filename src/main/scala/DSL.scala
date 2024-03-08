package dsl

enum Mode:
  case maj extends Mode
  case min extends Mode
end Mode 
/**
 *  Musical notes are named according to the LylyPond standard for english (https://lilypond.org/):
 *  - "a" to "g" for pitch
 *  - "s" for sharp, "f" for flat, "x" for double sharp and "ff" for double flat
 */
enum Note(val pitch: Int):
  case bs extends Note(0)
  case c extends Note(0)
  case dff extends Note(0)
  case bx extends Note(1)
  case cs extends Note(1)
  case df extends Note(1)
  case cx extends Note(2)
  case d extends Note(2)
  case eff extends Note(2)
  case ds extends Note(3)
  case ef extends Note(3)
  case fff extends Note(3)
  case dx extends Note(4)
  case e extends Note(4)
  case ff extends Note(4)
  case es extends Note(5)
  case f extends Note(5)
  case gff extends Note(5)
  case ex extends Note(6)
  case fs extends Note(6)
  case gf extends Note(6)
  case fx extends Note(7)
  case g extends Note(7)
  case aff extends Note(7)
  case gs extends Note(8)
  case af extends Note(8)
  case gx extends Note(9)
  case a extends Note(9)
  case bff extends Note(9)
  case as extends Note(10)
  case bf extends Note(10)
  case cff extends Note(10)
  case ax extends Note(11)
  case b extends Note(11)
  case cf extends Note(11)

  val diatonicScaleNumNote: Map[Int, String] = Map(0 -> "a", 1 -> "b", 2 -> "c", 3 -> "d", 4 -> "e", 5 -> "f", 6 -> "g")
  val diatonicScaleNoteNum: Map[String, Int] = Map("a" -> 0, "b" -> 1, "c" -> 2, "d" -> 3, "e" -> 4, "f" -> 5, "g" -> 6)
  def interval(interval: Interval): Note =
    val newPitch = (this.pitch + interval.semitones) % 12
    val newNoteKey = (diatonicScaleNoteNum.get(this.toString.substring(0,1)).get + interval.diatonic - 1) % 7
    val newNoteName: String = diatonicScaleNumNote.get(newNoteKey).get
    val newPitchNotes = Note.values.filter(_.pitch == newPitch)
    newPitchNotes.filter(_.toString.startsWith(newNoteName)).headOption.getOrElse(newPitchNotes.head)
end Note

enum Interval(val diatonic: Int, val semitones: Int):
  case min2 extends Interval(2, 1)
  case maj2 extends Interval(2, 2)
  case min3 extends Interval(3, 3)
  case maj3 extends Interval(3, 4)
  case dis5 extends Interval(5, 6)
  case perf5 extends Interval(5, 7)
  case aug5 extends Interval(5, 8)
  case dis7 extends Interval(7, 9)
  case min7 extends Interval(7, 10)
  case maj7 extends Interval(7,11)
  case perf8 extends Interval(1,0)
end Interval

type Chord = Vector[Note]

enum ChordFigure:
  case I extends ChordFigure()
  case V6 extends ChordFigure()
  case IV6 extends ChordFigure()
  case IV_IV extends ChordFigure()
  case V_IV extends ChordFigure()
  case IV extends ChordFigure()
  case V extends ChordFigure()
  case vi extends ChordFigure()
  case viiø extends ChordFigure()
  case I6 extends ChordFigure()
  case I64 extends ChordFigure()
  case V7 extends ChordFigure()
  case V6_vi extends ChordFigure()
  case V_V extends ChordFigure()
  case viio6_V extends ChordFigure()
  case V_vi extends ChordFigure()
  case vi6 extends ChordFigure()
  case ii65 extends ChordFigure()
  case ii7 extends ChordFigure()
  case ii extends ChordFigure()
  case iiø65_vi extends ChordFigure()
  case V7_vi extends ChordFigure()
  case iii6 extends ChordFigure()
  case vi64 extends ChordFigure()
  case V6_V extends ChordFigure()
  case vi7 extends ChordFigure()
  case ii6 extends ChordFigure()
  case viio6 extends ChordFigure()
  case I7 extends ChordFigure()
  case viiø42 extends ChordFigure()
  case viiø65 extends ChordFigure()
  case V7_VI extends ChordFigure()
  case VI extends ChordFigure()
  case V7_V extends ChordFigure()
  case V7_IV extends ChordFigure()
  case ii65_V extends ChordFigure()
  case V43_V extends ChordFigure()
  case V65_V extends ChordFigure()
  case viio_VI extends ChordFigure()
  case V42 extends ChordFigure()
  case viø43 extends ChordFigure()
  case viio43_vi extends ChordFigure()
  case V65_vi extends ChordFigure()
  case V7_bVII extends ChordFigure()
  case bVII extends ChordFigure()
  case V65 extends ChordFigure()
  case V65_ii extends ChordFigure()
  case V65_iii extends ChordFigure()
  case iii extends ChordFigure()
  case viio7_vi extends ChordFigure()
  case V65_v extends ChordFigure()
  case v extends ChordFigure()
  case V65_iv extends ChordFigure()
  case iv extends ChordFigure()
  case iv6 extends ChordFigure()
  case V_iv extends ChordFigure()
  case viio6_iv extends ChordFigure()
  case V43_iv extends ChordFigure()
  case V6_iv extends ChordFigure()
  case viio65_iv extends ChordFigure()
  case bVI_iv extends ChordFigure()
  case iv7 extends ChordFigure()
  case bIIIaug6_iv extends ChordFigure()
  case iv64 extends ChordFigure()
  case bVII6 extends ChordFigure()
  case bIIIaug6 extends ChordFigure()
  case i extends ChordFigure()
  case i6 extends ChordFigure()
  case iio64 extends ChordFigure()
  case v_iv extends ChordFigure()
  case iv6_iv extends ChordFigure()
  case viio7_bVII extends ChordFigure()
  case viio7 extends ChordFigure()
  case bIII extends ChordFigure()
  case V43 extends ChordFigure()
  case bVI extends ChordFigure()
  case i64 extends ChordFigure()
  case V6_v extends ChordFigure()
  case V_v extends ChordFigure()
  case viiø43_bVII extends ChordFigure()
  case viio6_bVII extends ChordFigure()
  case iio6 extends ChordFigure()
  case viio7_V extends ChordFigure()
  case iiio6 extends ChordFigure()
  case bII_IV extends ChordFigure()
  case viio65 extends ChordFigure()
  case V7_iv extends ChordFigure()
  case IV_bVI extends ChordFigure()
  case bVI6 extends ChordFigure()
  case V_bVI extends ChordFigure()
  case iv42 extends ChordFigure()
  case iv_iv extends ChordFigure()
  case V64 extends ChordFigure()
  case viio6_ii extends ChordFigure()
  case V7_ii extends ChordFigure()
  case IV7 extends ChordFigure()
  case bVI7 extends ChordFigure()
  case bIII6 extends ChordFigure()
  case V7_bIII extends ChordFigure()
  case iv65 extends ChordFigure()
  case iiø65 extends ChordFigure()
  case v6 extends ChordFigure()
  case V42_bVII extends ChordFigure()
  case IV65 extends ChordFigure()
  case iiø65_iv extends ChordFigure()
  case bVII7 extends ChordFigure()
  case bVI64 extends ChordFigure()
  case bVII7_ii extends ChordFigure()
  case iv6_ii extends ChordFigure()
  case bVII_vi extends ChordFigure()
  case V42_ii extends ChordFigure()
  case iv64_vi extends ChordFigure()
  case viio6_vi extends ChordFigure()
  case V42_IV extends ChordFigure()
  case iv_ii extends ChordFigure()
  case bVI6_ii extends ChordFigure()
  case iv65_vi extends ChordFigure()
  case IV65_vi extends ChordFigure()
  case ii64 extends ChordFigure()
  case V_ii extends ChordFigure()
  case viø extends ChordFigure()
  case viio extends ChordFigure()
  case V65_bVII extends ChordFigure()
  case bVI42 extends ChordFigure()
  case bIII7 extends ChordFigure()
  case bIII42 extends ChordFigure()
  case iv_v extends ChordFigure()
  case V7_v extends ChordFigure()
  case IV65_v extends ChordFigure()
  case bVII42 extends ChordFigure()
  case bIII64 extends ChordFigure()
  case iiø43 extends ChordFigure()
  case V43_bVII extends ChordFigure()
  case v7 extends ChordFigure()
  case bVI6_v extends ChordFigure()
  case bIII7_v extends ChordFigure()
  case IV6_v extends ChordFigure()
  case bIIIaug_iv extends ChordFigure()
  case IV42 extends ChordFigure()
  case IV_V extends ChordFigure()
  case ii7_V extends ChordFigure()
  case iv7_V extends ChordFigure()
  case bIIIaug_V extends ChordFigure()
  case ii42 extends ChordFigure()
  case I65 extends ChordFigure()
  case viio65_ii extends ChordFigure()
  case V6_ii extends ChordFigure()
  case ii6_V extends ChordFigure()
  case V42_V extends ChordFigure()
  case IV65_V extends ChordFigure()
  case viio43_v extends ChordFigure()
  case viio7_v extends ChordFigure()
  case iiø65_v extends ChordFigure()
  case i42 extends ChordFigure()
  case viio42 extends ChordFigure()
  case V65_IV extends ChordFigure()
  case ii_V extends ChordFigure()
  case bIIIaug6_ii extends ChordFigure()
  case iiø42_v extends ChordFigure()
  case IV6_vi extends ChordFigure()
  case viio43 extends ChordFigure()
  case IV_bIII extends ChordFigure()
  case iiø43_bIII extends ChordFigure()
  case V6_bVII extends ChordFigure()
  case ii65_bIII extends ChordFigure()
  case bVII64 extends ChordFigure()
  case V_bVII extends ChordFigure()
  case v43 extends ChordFigure()
  case vi65 extends ChordFigure()
  case vio6 extends ChordFigure()
  case iii7 extends ChordFigure()
  case vii6 extends ChordFigure()
  case iiø42 extends ChordFigure()
  case viio7_ii extends ChordFigure()
  case bVII65 extends ChordFigure()
  case viio_bVII extends ChordFigure()
  case V7_bVI extends ChordFigure()
  case iv7_vii extends ChordFigure()
  case V7_vii extends ChordFigure()
  case vii extends ChordFigure()
  case V64_bVII extends ChordFigure()
  case vi_bVII extends ChordFigure()
  case iiø6_v extends ChordFigure()
  case bIIIaug65 extends ChordFigure()
  case iv7_v extends ChordFigure()
  case V42_v extends ChordFigure()
  case iiø extends ChordFigure()
  case ii_v extends ChordFigure()
  case viiø_bVII extends ChordFigure()
  case viio6_v extends ChordFigure()

end ChordFigure