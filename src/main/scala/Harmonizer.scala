package generator

import dsl.*
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.language.postfixOps

val NICE_MOV_SCORE = 2
val BAD_MOV_SCORE = -1


def harmonizeChoral(choral: GeneratedChoral): HarmonizedChoral =
  HarmonizedChoral(choral.semiphrases.zipWithIndex.map: (sp, idx) =>
//    println(s"\n----------- Harmonizing semiphrase ${idx + 1} -----------\n")
    harmonizeSemiphrase(sp)
    , choral)

def harmonizeSemiphrase(semiphrase: GeneratedSemiphrase): HarmonizedSemiphrase =
  val bassLine = generateBassLine(semiphrase.map(_.bass))
  generateHarmonizedSemiphrase(bassLine, semiphrase)

def generateBassLine(bassNotes: Vector[Note]): Vector[NoteWithOctave] =
  @tailrec
  def rec(notes: Vector[Note], acc: Vector[NoteWithOctave]): Vector[NoteWithOctave] = notes match
    case x +: xs => rec(xs, acc :+ acc.last.nearestNoteInRange(x, VoiceRange.bass))
    case _ => acc

  @tailrec
  def review(tail: Vector[NoteWithOctave], acc: Vector[NoteWithOctave]): Vector[NoteWithOctave] = tail match
    case x1 +: x2 +: x3 +: xs if x1 equals x2 =>
      if x2.lowerOctave.isInRange(VoiceRange.bass) then
        if math.abs(x2.lowerOctave.absolutPitch - x3.absolutPitch) > 12 then review(xs, acc :+ x1 :+ x2.lowerOctave :+ x3.lowerOctave)
        else review(xs, acc :+ x1 :+ x2.lowerOctave :+ x3)
      else review(tail.tail, acc :+ x1)
    case x1 +: x2 +: x3 +: xs => review(tail.tail, acc :+ x1)
    case x2 +: x3 +: Vector() if x2.absolutPitch > x3.absolutPitch =>
      if x2.lowerOctave.isInRange(VoiceRange.bass) then
//        println(s"$x2 lowed going to $x3")
        acc :+ x2.lowerOctave :+ x3
      else if x3.upperOctave.isInRange(VoiceRange.bass) then
//        println(s"$x3 upped arriving from $x2")
        acc :+ x2 :+ x3.upperOctave
      else
//        println(s"None moved in $x2 to $x3")
        acc :+ x2 :+ x3
    case _ => acc ++ tail

  val bassLine = rec(bassNotes.tail, Vector(NoteWithOctave.lowestOctaveInRange(bassNotes.head, VoiceRange.bass)))
  review(bassLine.tail, Vector(bassLine.head))

def unisons(curr: HarmonizedChord): Boolean =
  curr.bass == curr.tenor || curr.tenor == curr.contra || curr.contra == curr.treble
def crossedVoices(curr: HarmonizedChord): Boolean =
  curr.bass.absolutPitch > curr.tenor.absolutPitch || curr.tenor.absolutPitch > curr.contra.absolutPitch || curr.contra.absolutPitch > curr.treble.absolutPitch
def wrongVoicesDistance(curr: HarmonizedChord): Boolean = {
  val distanceTenorContra = curr.contra.absolutPitch - curr.tenor.absolutPitch
  val distanceContraTreble = curr.treble.absolutPitch - curr.contra.absolutPitch
  distanceTenorContra > 12 || distanceTenorContra < 2 || distanceContraTreble > 12 || distanceContraTreble < 2
}
def isValidChordHarmonization(curr: HarmonizedChord, indent: String): Boolean =
  val rulesBroken = unisons(curr) || crossedVoices(curr) || wrongVoicesDistance(curr)
  !rulesBroken
def parallelMovements(curr: HarmonizedChord, prev: HarmonizedChord, indent: String): Boolean =
  def rec(prevVoices: Vector[NoteWithOctave], currVoices: Vector[NoteWithOctave]): Boolean = (prevVoices, currVoices) match {
    case (x +: Vector(), _) => false
    case (x +: xs, y +: ys) =>
      val pairedMovements = xs.map(x.getInterval(_)).lazyZip(ys.map(y.getInterval(_)))
      val error: Option[(Interval, Interval)] = pairedMovements.find((i1, i2) => (i1 isFifthOrEighth) && (i1 equals i2))
      if error isDefined then
//        println(s"${indent}ERROR: Parallel movement found between chords $prev and $curr")
        true
      else rec(xs, ys)
  }

  val currChordSeq = Vector(curr.bass, curr.tenor, curr.contra, curr.treble)
  val prevChordSeq = Vector(prev.bass, prev.tenor, prev.contra, prev.treble)
  rec(prevChordSeq, currChordSeq)
def seventhResolution(curr: HarmonizedChord, prev: HarmonizedChord, prevChord: Chord, indent: String): Boolean =
  if prevChord.seventh.isDefined then
    val pairedMovements = Vector(prev.tenor, prev.contra, prev.treble) lazyZip Vector(curr.tenor, curr.contra, curr.treble)
    val seventhUnresolved = pairedMovements.find((x, y) =>
      (x.note equals prevChord.seventh.get) && (x.absolutPitch - y.absolutPitch > 2 || x.absolutPitch - y.absolutPitch < 1)
    )
    if seventhUnresolved isDefined then
//      println(s"${indent}ERROR: Unresolved seventh found between chords $prev and $curr")
      true
    else false
  else false
def isValidLink(curr: HarmonizedChord, prev: HarmonizedChord, prevChord: Chord, indent: String): Boolean =
  val rulesBroken = parallelMovements(curr, prev, indent) || seventhResolution(curr, prev, prevChord, indent)
  !rulesBroken
def computeLinkScore(curr: HarmonizedChord, prev: HarmonizedChord, indent: String): Int =
  import IntervalDirection._
  val pairedMovements = Vector(prev.tenor, prev.contra, prev.treble) zip Vector(curr.tenor, curr.contra, curr.treble)
  val (upper, lower, bassMovement) = if prev.bass - curr.bass > 0 then (prev.bass, curr.bass, desc) else (curr.bass, prev.bass, asc)
  lower.getInterval(upper) match {
    case Interval(diatonic, _) if diatonic == 2 => pairedMovements.map { (x, y) =>
      val thisMovement = if x - y > 0 then desc else asc
      if thisMovement isCounterMovementTo bassMovement then NICE_MOV_SCORE else BAD_MOV_SCORE
    }.sum
    case Interval(diatonic, _) if diatonic > 2 => pairedMovements.map { (x, y) =>
      x - y match {
        case 0 => 5
        case 1 => 3
        case 2 => 1
        case 3 => 0
        case _ => -4
      }
    }.sum
    case _ => 0
  }
def generateHarmonizedSemiphrase(bassLine: Vector[NoteWithOctave], semiphrase: GeneratedSemiphrase): HarmonizedSemiphrase = {
  def rec(
           bassLine: Vector[NoteWithOctave],
           chords: GeneratedSemiphrase,
           lastHarmonizedChord: HarmonizedChord,
           lastChord: Option[Chord],
           acc: Vector[HarmonizedChord],
           recLevel: Int
         ): LazyList[HarmonizedSemiphrase] =
    def indentCalc(n: Int): String = if n == 0 then "" else s"  ${indentCalc(n - 1)}"
    val indent = indentCalc(recLevel)
    (bassLine, chords) match {
      case (bass +: bs, chord +: cs) =>
        val tenorNotes = chord.notes orderByNearness(lastHarmonizedChord.tenor.absolutPitch, VoiceRange.tenor)
        val contraNotes = chord.notes orderByNearness(lastHarmonizedChord.contra.absolutPitch, VoiceRange.contra)
        val trebleNotes = chord.notes orderByNearness(lastHarmonizedChord.treble.absolutPitch, VoiceRange.treble)
        println(s"${indent}$recLevel| Starting links search for ${chord.figure}, acc: ${acc.length}, chords left:${chords.length}")
        val validAttempts = for {
          tenor <- tenorNotes
          contra <- contraNotes
          treble <- trebleNotes
          attempt = HarmonizedChord(bass, tenor, contra, treble)
          if isValidChordHarmonization(attempt, indent)
          if lastChord.isEmpty || isValidLink(attempt, lastHarmonizedChord, lastChord.get, indent)
          score = if lastChord.isEmpty then 1 else computeLinkScore(attempt, lastHarmonizedChord, indent)
        } yield (attempt, score)
        val orderedValidAttempts = validAttempts.sortBy(_._2)(Ordering[Int].reverse).map(_._1)
        println(s"${indent}$recLevel| SCORE COMPUTING: \n$indent\tScored vector: $validAttempts \n$indent\tordered vector: $orderedValidAttempts")
        orderedValidAttempts to LazyList flatMap (attempt => rec(bs, cs, attempt, Some(chord), acc :+ attempt, recLevel + 1))

      case _ => LazyList(HarmonizedSemiphrase(acc))
    }

  val middleBoundsReference = HarmonizedChord(
    VoiceRange.bass.middleNote,
    VoiceRange.tenor.middleNote,
    VoiceRange.contra.middleNote,
    VoiceRange.treble.middleNote
  )
  rec(bassLine, semiphrase, middleBoundsReference, None, Vector(), 1).head
}

