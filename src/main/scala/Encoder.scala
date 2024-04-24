package dsl

import model._

def encodeToLilypond(choral: GeneratedChoral): String =
  val chords = choral.semiphrases.flatMap(semiphrase =>
    val start = s"\t${semiphrase.head.toString}4"
    val body = semiphrase.tail.dropRight(1).map(_.toString)
    val end = s"${semiphrase.last.toString}4\\fermata \n"
    start +: body :+ end
  )
  val finalChord = chords.last
  val finalDuration = chords.size % 4 match
    case 1 => "1"
    case 2 => "2."
    case 3 => "2"
    case _ => "4"
  val lyrics: String = choral.semiphrases
    .map(_.map(c => s"\"${
      c.figure.toString
        .replace("semdis", "ø")
        .replace("dis", "o")
        .replace("aug", "+")
        .replace("_", "/")
    }\""
    ).mkString(" ")).mkString(" ")
  val music = s"\\version \"2.24.3\"\n<<\n\\relative { \n${chords.dropRight(1).mkString(" ")} ${finalChord.replace("4\\fermata", s"$finalDuration\\fermata")}\n } \n \\addlyrics { \n$lyrics \n } \n>>"
  music


def encodeToScalaDataStructures(objectName: String, model: Model): String =
  s"""package generator
  import dsl.ChordFigure.*
  import model.{Model,SemiphraseModel}

  val $objectName = Model(
    ${model.initialSemiphrase.toDataStructureCode},
    ${model.middleSemiphrases.toDataStructureCode},
    ${model.lastSemiphrase.toDataStructureCode},
    ${model.endingToInitialChordsTransitions.fotToDataStructureCode},
    (${model.middleSectionBounds._1}, ${model.middleSectionBounds._2})
  )"""