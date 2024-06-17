package generator

import dsl.Note
import model.Model
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.comcast.ip4s.{Port, ipv4, port}
import org.http4s.{HttpApp, HttpRoutes, StaticFile, Status}
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.ember.server.*
import fs2.io.file.Path
import org.http4s.circe.CirceEntityCodec.*
import io.circe.generic.auto.*

import scala.util.{Failure, Random, Success, Try}
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
        val key = Try(Note.valueOf(inputKey.trim)).getOrElse(Note.c)
        val model = if inputMode.trim.equals("major") then majorModel else minorModel
        val pdfId = r.nextInt.toString
        val midId = r.nextInt.toString
        val lilypondToPdfPath = s"${outputPath}choral$pdfId.ly"
        val lilypondToMidiPath = s"${outputPath}choral$midId.ly"
        val generation = Try {
          val choral = model.generateChoral(r, key)
          writeTextToFile(lilypondToPdfPath, harmonizeChoral(choral).toLilypondFileFormat(false))
          writeTextToFile(lilypondToMidiPath, harmonizeChoral(choral).toLilypondFileFormat(true))
        }
        val lilypondProcessed = Try {
          s"lilypond -fpdf $lilypondToPdfPath $lilypondToMidiPath".!!
        }
        (generation, lilypondProcessed) match {
          case (Success(_), Success(_)) =>  Ok (FileUrls (pdf = pdfId, midi = midId))
          case (Success(_), Failure(_)) => InternalServerError ("Failed at processing generated choral with Lilypond")
          case (Failure(_), _) => InternalServerError ("Failed at generating choral")
        }

      case request@GET -> Root / "midi" / fileId =>
        StaticFile.fromPath(Path(s"choral$fileId.mid"), Some(request)).getOrElseF(NotFound())

      case request@GET -> Root / "pdf" / fileId =>
        StaticFile.fromPath(Path(s"choral$fileId.pdf"), Some(request)).getOrElseF(NotFound())
      case request@DELETE -> Root / fileId =>
        val cleanUp = Try {
          s"rm choral$fileId.* ".!!
          s"rm ${outputPath}choral$fileId.ly".!!
        }
        if cleanUp.isSuccess then Ok()
        else InternalServerError("Deletion not completed")
    }.orNotFound

  def readModelFromFile(path: String): IO[Model] =
    Resource
      .fromAutoCloseable(IO(new ObjectInputStream(new FileInputStream(path))))
      .use(input => IO(input.readObject().asInstanceOf[Model]))

  def writeTextToFile(path: String, choral: String): Unit =
    val writer = new FileWriter(path)
    writer.write(choral)
    writer.close()


  def run(args: List[String]): IO[ExitCode] = for {
    _ <- Parser.program
    port <- IO(Port.fromString(sys.env.getOrElse("PORT", "8080")).get)
    minorModel <- readModelFromFile(s"${modelsPath}minor.model")
    majorModel <- readModelFromFile(s"${modelsPath}major.model")
    exit <- EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port)
      .withHttpApp(choralGenerator(minorModel, majorModel))
      .build
      .use(_ => IO.never)
      .as(ExitCode.Success)
  } yield exit



}

