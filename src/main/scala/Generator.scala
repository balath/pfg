package generator

import cats.effect.{ExitCode, IO, IOApp, Resource}
import common.DataRegex.{dataPath, modelsPath, outputPath}
import dsl.{Choral, Mode, Note, encodeToLilypond}
import model.Model

import java.io.{FileInputStream, FileOutputStream, FileWriter, ObjectInputStream, ObjectOutputStream}
import scala.util.Random

object Generator extends IOApp {

  val r = new Random

  override def run(args: List[String]): IO[ExitCode] = program

  val program = for {
    majorModel <- readModelFromFile(s"${modelsPath}major.model")
    _ <- writeTextToFile(s"$outputPath/major${r.nextInt()}.ly", encodeToLilypond(majorModel.generateChoral(r, Note.c)))
    //      >> writeTextToFile(s"$outputPath/minor${r.nextInt()}.ly", encodeToLilypond(minorModel.generateChoral(r, Note.c)))
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
