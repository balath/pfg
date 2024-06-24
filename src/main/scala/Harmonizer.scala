package generator

import dsl.*
import java.time.LocalDateTime
import scala.annotation.tailrec
import scala.language.postfixOps

val NICE_MOV_SCORE = 3
val BAD_MOV_SCORE = 0
val INTERVAL_COMPLEMENT = 9
val DIS_AUG_PENALTY = 0.7
val LOG_ENABLED = false


def harmonizeChoral(choral: GeneratedChoral): HarmonizedChoral =
  val harmonizedChoral = HarmonizedChoral(choral.semiphrases.zipWithIndex.map{(s,i) =>
    println(s"Harmonizing semiphrase ${i+1}")
    harmonizeSemiphrase(s)
  }, choral)
  println("Choral successfully harmonized!!!")
  harmonizedChoral

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
        acc :+ x2.lowerOctave :+ x3
      else if x3.upperOctave.isInRange(VoiceRange.bass) then
        acc :+ x2 :+ x3.upperOctave
      else
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
def duplications(curr: HarmonizedChord, currChord: Chord): Int =
  val voices = Vector(curr.bass, curr.tenor, curr.contra, curr.treble)
  val niceDuplications = if currChord.seventh.isEmpty then
    if currChord.bass equals currChord.third then !voices.tail.exists(_.note equals currChord.third)
    else voices.tail.count(_.note equals currChord.third) > 1
  else
    if currChord.bass equals currChord.tonic then voices.tail.count(_.note equals currChord.tonic) > 1 && !voices.tail.exists(_.note equals currChord.fifth)
    else voices.map(_.note).distinct.size == voices.size
  val disFifth = if currChord.isDisminished then voices.tail.count(_.note.equals(currChord.fifth)) < 2 else true
  Vector(disFifth, niceDuplications).map(if _ then 6 else -6).sum
def isValidChordHarmonization(curr: HarmonizedChord, indent: String): Boolean =
  val rulesBroken = unisons(curr) || crossedVoices(curr) || wrongVoicesDistance(curr)
  !rulesBroken

def parallelMovements(curr: HarmonizedChord, prev: HarmonizedChord, currChord: Chord, prevChord: Chord, indent: String): Boolean =
  def rec(prevVoices: Vector[NoteWithOctave], currVoices: Vector[NoteWithOctave]): Boolean = (prevVoices, currVoices) match {
    case (x +: Vector(), _) => false
    case (x +: xs, y +: ys) =>
      val pairedMovements = xs.map(x.getMelodicInterval(_)).lazyZip(ys.map(y.getMelodicInterval(_)))
      val error: Option[(Interval, Interval)] = pairedMovements.find((i1, i2) => (i1 isFifthOrEighth) && (i1 equals i2))
      if error isDefined then
        if LOG_ENABLED then
              println(s"${indent}ERROR: Parallel movement found between chords $prev and $curr")
        true
      else rec(xs, ys)
    case (_,_) => false
  }
  if currChord.grade.isDominant && prevChord.grade.isDominant then false
  else
    val currChordSeq = Vector(curr.bass, curr.tenor, curr.contra, curr.treble)
    val prevChordSeq = Vector(prev.bass, prev.tenor, prev.contra, prev.treble)
    rec(prevChordSeq, currChordSeq)
def leadingNoteResolution(curr: HarmonizedChord, prev: HarmonizedChord, prevChord: Chord, indent: String): Boolean =
  if prevChord.grade.isDominant then
    val pairedMovements = Vector(prev.tenor, prev.contra, prev.treble) lazyZip Vector(curr.tenor, curr.contra, curr.treble)
    val result = pairedMovements.exists((x, y) => x.note.equals(prevChord.leadingNote) && !x.note.interval(Interval.min2).equals(y.note))
    if result && LOG_ENABLED then
        println(s"${indent}ERROR: Leading note (${prevChord.leadingNote}) bad resolution between chords $prev and $curr")
    result
  else false
def aumentedOrDisminished(curr: HarmonizedChord, prev: HarmonizedChord, currChord: Chord, prevChord: Chord, indent: String): Boolean =
  if currChord.grade.isDominant && prevChord.grade.isDominant then false
  else
    val pairedMovements = Vector(prev.tenor, prev.contra, prev.treble) lazyZip Vector(curr.tenor, curr.contra, curr.treble)
    val result = pairedMovements.exists((x, y) => x.getMelodicInterval(y).isDisminishedOrAugmented)
    if result && LOG_ENABLED then
        println(s"${indent}ERROR: Dis or Aug melodic interval between chords $prev and $curr")
    result
def seventhResolution(curr: HarmonizedChord, prev: HarmonizedChord, prevChord: Chord, indent: String): Boolean =
  if prevChord.seventh.isDefined then
    val pairedMovements = Vector(prev.tenor, prev.contra, prev.treble) lazyZip Vector(curr.tenor, curr.contra, curr.treble)
    val seventhUnresolved = pairedMovements.find((x, y) =>
      (x.note equals prevChord.seventh.get) && (x.absolutPitch - y.absolutPitch > 2 || x.absolutPitch - y.absolutPitch < 1)
    )
    if seventhUnresolved isDefined then
      if LOG_ENABLED then
          println(s"${indent}ERROR: Unresolved seventh found between chords $prev and $curr")
      true
    else false
  else false
def isValidLink(curr: HarmonizedChord, prev: HarmonizedChord, currChord: Chord, prevChord: Chord, indent: String): Boolean =
  val rulesBroken = parallelMovements(curr, prev, currChord, prevChord, indent)
    || seventhResolution(curr, prev, prevChord, indent)
    || leadingNoteResolution(curr, prev, prevChord, indent)
  !rulesBroken
def computeLinkScore(curr: HarmonizedChord, prev: HarmonizedChord, indent: String): Int =
  val pairedMovements = Vector(prev.tenor, prev.contra, prev.treble) zip Vector(curr.tenor, curr.contra, curr.treble)
  val bassMovement = IntervalDirection(prev.bass, curr.bass)
  prev.bass.getMelodicInterval(curr.bass) match {
    case Interval(diatonic, _) if diatonic == 2 => pairedMovements.map { (x, y) =>
      val thisMovement = IntervalDirection(x,y)
      val counterMoveScore = if (thisMovement isCounterMovementTo bassMovement) || thisMovement.equals(IntervalDirection.hold) then NICE_MOV_SCORE else BAD_MOV_SCORE
      val intervalScore = INTERVAL_COMPLEMENT - x.getMelodicInterval(y).diatonic
      counterMoveScore + intervalScore
    }.sum
    case _ => pairedMovements.map { (x, y) =>
      INTERVAL_COMPLEMENT - x.getMelodicInterval(y).diatonic
    }.sum
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
    val indent = "  ".repeat(recLevel)
    (bassLine, chords) match {
      case (bass +: bs, chord +: cs) =>
        val tenorNotes = chord.notes orderByNearness(lastHarmonizedChord.tenor.absolutPitch, VoiceRange.tenor)
        val contraNotes = chord.notes orderByNearness(lastHarmonizedChord.contra.absolutPitch, VoiceRange.contra)
        val trebleNotes: Vector[NoteWithOctave] = chord.notes orderByNearness(lastHarmonizedChord.treble.absolutPitch, VoiceRange.treble)
        if LOG_ENABLED then
          println(s"$indent$recLevel| Starting links search for ${chord.figure}, acc: ${acc.length}, chords left:${chords.length}")
        val validAttempts = for {
          tenor <- tenorNotes
          contra <- contraNotes
          treble <- trebleNotes
          attempt = HarmonizedChord(bass, tenor, contra, treble)
          if isValidChordHarmonization(attempt, indent)
          if lastChord.isEmpty || isValidLink(attempt, lastHarmonizedChord, chord, lastChord.get, indent)
          score = if lastChord.isEmpty then 1 else computeLinkScore(attempt, lastHarmonizedChord, indent)
          disOrAug = if lastChord.isDefined && aumentedOrDisminished(attempt, lastHarmonizedChord, chord, lastChord.get, indent) then DIS_AUG_PENALTY else 1
          niceDuplic = duplications(attempt, chord)
        } yield (attempt, (score + niceDuplic) * disOrAug)
        val orderedAttemptsWithScore = validAttempts.sortBy(_._2)(Ordering[Double].reverse)
        if LOG_ENABLED then
          println(s"$indent$recLevel| SCORE COMPUTING: $indent\tordered vector: $orderedAttemptsWithScore")
        val orderedAttempts = orderedAttemptsWithScore.map(_._1)
        orderedAttempts to LazyList flatMap (attempt => rec(bs, cs, attempt, Some(chord), acc :+ attempt, recLevel + 1))

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

