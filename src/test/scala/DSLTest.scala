import munit.FunSuite
import dsl._

class DSLTest extends FunSuite{

  //    Clue for one by one interval test
  //    val clue = s"Wrong ${interval.toString} interval for ${note.toString}: expected ${expected.toString} but obtained ${obtained.toString}"

  val majorModesNotes = Vector(
    Vector(Note.c, Note.d, Note.e, Note.f, Note.g, Note.a, Note.b),
    Vector(Note.cs, Note.ds, Note.es, Note.fs, Note.gs, Note.as, Note.bs),
    Vector(Note.df, Note.ef, Note.f, Note.gf, Note.af, Note.bf, Note.c),
    Vector(Note.d, Note.e, Note.fs, Note.g, Note.a, Note.b, Note.cs),
    Vector(Note.ef, Note.f, Note.g, Note.af, Note.bf, Note.c, Note.d),
    Vector(Note.e, Note.fs, Note.gs, Note.a, Note.b, Note.cs, Note.ds),
    Vector(Note.f, Note.g, Note.a, Note.bf, Note.c, Note.d, Note.e),
    Vector(Note.fs, Note.gs, Note.as, Note.b, Note.cs, Note.ds, Note.es),
    Vector(Note.gf, Note.af, Note.bf, Note.cf, Note.df, Note.ef, Note.f),
    Vector(Note.g, Note.a, Note.b, Note.c, Note.d, Note.e, Note.fs),
    Vector(Note.af, Note.bf, Note.c, Note.df, Note.ef, Note.f, Note.g),
    Vector(Note.a, Note.b, Note.cs, Note.d, Note.e, Note.fs, Note.gs),
    Vector(Note.bf, Note.c, Note.d, Note.ef, Note.f, Note.g, Note.a),
    Vector(Note.b, Note.cs, Note.ds, Note.e, Note.fs, Note.gs, Note.as),
  )

  val minorModesNotes = for {
    mode <- majorModesNotes
    (a, b) = mode.splitAt(5)
  } yield b++a

  test("Perfect and majors intervals on major modes tonics should result on major mode notes") {
    val majorAndPerfectIntervals = Vector(Interval.maj2, Interval.maj3, Interval.perf5, Interval.maj7)
    val obtained = majorModesNotes.map(mode => (majorAndPerfectIntervals.map(mode.head.interval(_)), mode))
    assert(obtained.forall((results, mode) => results.forall(mode.contains(_))), obtained.toString)
  }

  test("Minor, diminished and augmented intervals on major modes tonics should not result on major mode notes") {
    val minorDimAndAugIntervals = Vector(Interval.min2, Interval.min3, Interval.dis5, Interval.aug5, Interval.dis7, Interval.min7)
    val obtained = majorModesNotes.map(mode => (minorDimAndAugIntervals.map(mode.head.interval(_)), mode))
    assert(obtained.forall((results, mode) => results.forall(!mode.contains(_))), obtained.toString)
  }

  test("Minor mode intervals on minor modes tonics should result on minor mode notes") {
    val minorModeIntervals = Vector(Interval.maj2, Interval.min3, Interval.perf5, Interval.min7)
    val obtained = minorModesNotes.map(mode => (minorModeIntervals.map(mode.head.interval(_)), mode))
    assert(obtained.forall((results, mode) => results.forall(mode.contains(_))), obtained.toString)
  }

  test("Intervals over c and over g are properly obtained") {
    val cExpected: Vector[Note] = Vector(Note.df, Note.d, Note.ef, Note.e, Note.f, Note.gf, Note.g, Note.gs, Note.af, Note.a, Note.bff, Note.bf, Note.b, Note.c)
    val gExpected: Vector[Note] = Vector(Note.af, Note.a, Note.bf, Note.b, Note.c, Note.df, Note.d, Note.ds, Note.ef, Note.e, Note.ff, Note.f, Note.fs, Note.g)
    val intervals = Interval.values.toVector

    val cObtained: Vector[Note] = intervals.map(interval => {
      val note = Note.c.interval(interval)
      println(s"An $interval over c is a $note")
      note
    })
    val gObtained: Vector[Note] = intervals.map(interval => {
      val note = Note.g.interval(interval)
      println(s"An $interval over g is a $note")
      note
    })
    cObtained.zip(cExpected).foreach(tuple => assertEquals(tuple._1, tuple._2))
    gObtained.zip(gExpected).foreach(tuple => assertEquals(tuple._1, tuple._2))
  }

}
