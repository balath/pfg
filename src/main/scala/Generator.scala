package generator

import cats.effect.{ExitCode, IO, IOApp, Resource}
import common.DataRegex.{dataPath, modelsPath, outputPath}
import dsl.{Choral, Mode, Note}
import model.Model
import scala.sys.process._
import java.io.{FileInputStream, FileOutputStream, FileWriter, ObjectInputStream, ObjectOutputStream}
import scala.util.Random

object Generator extends IOApp {

  val r = new Random

  override def run(args: List[String]): IO[ExitCode] = program

  val program = for {
    model <- readModelFromFile(s"${modelsPath}minor.model")
    sample = model.generateChoral(r, Note.c)
//    _ <- IO.println(harmonizeChoral(sample).toLilypondFileFormat(true))
//    _ <- IO.println(harmonizeChoral(sample).toLilypondFileFormat(false))
    path <- IO(s"$outputPath/sample${r.nextInt()}.ly")
    _ <- writeTextToFile(path, harmonizeChoral(sample).toLilypondFileFormat(true))
    _ <- IO(s"lilypond -fpdf $path".!!)
  } yield ExitCode.Success

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
