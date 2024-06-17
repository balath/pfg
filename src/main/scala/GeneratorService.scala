package generator

import dsl.Note
import model.Model
import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.{ipv4, port}
import org.http4s.{HttpApp, HttpRoutes, StaticFile, Status}
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.ember.server.*
import fs2.io.file.Path
import org.http4s.circe.CirceEntityCodec.*
import io.circe.generic.auto.*
import scala.util.{Random, Try}
import scala.sys.process.stringToProcess
import java.io.{FileInputStream, FileWriter, ObjectInputStream}

object GeneratorService extends IOApp {

  case class FileUrls(pdf: String, midi: String)
  val modelsPath = "models/"
  val outputPath = "output/"
  val r = new Random
  def choralGenerator(minorModel: Model, majorModel: Model): HttpApp[IO] =
    HttpRoutes.of[IO] {
      case request@GET -> Root / "choral" / inputKey / inputMode =>
        val key = Note.valueOf(inputKey.trim)
        val model = if inputMode.trim.equals("major") then majorModel else minorModel
        val pdfId = r.nextInt.toString
        val midId = r.nextInt.toString
        val lilypondToPdfPath = s"${outputPath}choral$pdfId.ly"
        val lilypondToMidiPath = s"${outputPath}choral$midId.ly"
        val generation = Try {
          val choral = model.generateChoral(r, key)
          if key.equals(Note.b) then throw new Exception("boo")
          writeTextToFile(lilypondToPdfPath, harmonizeChoral(choral).toLilypondFileFormat(false))
          writeTextToFile(lilypondToMidiPath, harmonizeChoral(choral).toLilypondFileFormat(true))
          s"lilypond -fpdf $lilypondToPdfPath $lilypondToMidiPath".!!
        }
        if generation.isSuccess then Ok(FileUrls(pdf = pdfId, midi = midId))
        else InternalServerError("Failed to generate choral")

      case request@GET -> Root / "midi" / fileName =>
        StaticFile.fromPath(Path(s"choral$fileName.mid"), Some(request)).getOrElseF(NotFound())

      case request@GET -> Root / "pdf" / fileName =>
        StaticFile.fromPath(Path(s"choral$fileName.pdf"), Some(request)).getOrElseF(NotFound())
    }.orNotFound

  def readModelFromFile(path: String): Model =
    new ObjectInputStream(new FileInputStream(path)).readObject().asInstanceOf[Model]

  def writeTextToFile(path: String, choral: String): Unit =
    val writer = new FileWriter(path)
    writer.write(choral)
    writer.close()


  def run(args: List[String]): IO[ExitCode] = for {
    _ <- Parser.program
    minorModel = readModelFromFile(s"${modelsPath}minor.model")
    majorModel = readModelFromFile(s"${modelsPath}major.model")
    exit <- EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(choralGenerator(minorModel, majorModel))
    .build
    .use(_ => IO.never)
    .as(ExitCode.Success)
  } yield exit
}