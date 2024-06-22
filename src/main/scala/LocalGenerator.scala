package generator

import cats.effect.{ExitCode, IO, IOApp, Resource}
import common.DataRegex.{dataPath, modelsPath, outputPath}
import dsl.{Choral, Mode, Note}
import model.Model
import scala.sys.process._
import java.io.{FileInputStream, FileOutputStream, FileWriter, ObjectInputStream, ObjectOutputStream}
import scala.util.Random
object LocalGenerator extends IOApp {

  val r = new Random
  override def run(args: List[String]): IO[ExitCode] = program

  val program: IO[ExitCode] = for {
      minorModel <- readModelFromFile(s"${modelsPath}minor.model")
      majorModel <- readModelFromFile(s"${modelsPath}major.model")
      _ <- generationMenu(minorModel, majorModel)//.foreverM
    } yield ExitCode.Success

  def generationMenu(minorModel: Model, majorModel: Model): IO[Unit] =
    for {
//      _ <- IO.print("Tono (c|d|e|f|g|a|b): ")
//      inputKey <- IO.readLine
//      _ <- IO.print("Modo (major|minor): ")
//      inputMode <- IO.readLine
//      key = Note.valueOf(inputKey.trim)
//      _ <- IO.println(s"Key = $key")
//      mode = if inputMode.trim.equals("major") then majorModel else minorModel
      key <- IO(Note.aes)
      mode <- IO(Mode.min)
      choral <- IO(minorModel.generateChoral(r, key, mode))
      pdfPath <- IO(s"$outputPath/choral${r.nextInt()}.ly")
      midiPath <- IO(s"$outputPath/choral${r.nextInt()}.ly")
      _ <- writeTextToFile(pdfPath, harmonizeChoral(choral).toLilypondFileFormat(false))
      _ <- writeTextToFile(midiPath, harmonizeChoral(choral).toLilypondFileFormat(true))
      _ <- IO(s"lilypond -fpdf $pdfPath $midiPath".!!)
    } yield IO.unit

  def readModelFromFile(path: String): IO[Model] =
    Resource
      .fromAutoCloseable(IO(new ObjectInputStream(new FileInputStream(path))))
      .use(input => IO(input.readObject().asInstanceOf[Model]))

  def writeTextToFile(path: String, choral: String): IO[Unit] =
    Resource
      .make {
        IO(new FileWriter(path))
      } { writer => IO(writer.close()).handleErrorWith(_ => IO.unit) }
      .use { writer => IO(writer.write(choral)) }
}
