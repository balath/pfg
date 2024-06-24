package generator

import dsl.{Note, Mode}
import model.Model
import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.comcast.ip4s.{Port, ipv4, port}
import org.http4s.{HttpApp, HttpRoutes, StaticFile, Status, Uri}
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.ember.server.*
import fs2.io.file.Path
import org.http4s.circe.CirceEntityCodec.*
import io.circe.generic.auto.*
import org.http4s.headers.Origin
import org.http4s.server.middleware.{CORS, CORSConfig}
import concurrent.duration.DurationInt
import scala.util.{Failure, Random, Success, Try}
import scala.sys.process.{ProcessLogger, stringToProcess}
import java.io.{FileInputStream, FileWriter, ObjectInputStream}

object GeneratorService extends IOApp {

  case class FileUrls(pdf: String, midi: String)
  val modelsPath = "models/"
  val outputPath = "output/"
  val initialAttempts = 3
  val r = new Random

  def corsService(minorModel: Model, majorModel: Model, attempts: Int): HttpApp[IO] =
    CORS.policy
      .withAllowOriginHost(Set(
        Origin.Host(Uri.Scheme.https, Uri.RegName("balath.github.io"), None),
        Origin.Host(Uri.Scheme.https, Uri.RegName("balath.github.io"), None)
      ))
      .withAllowCredentials(true)
      .withMaxAge(1.day)
      .apply(choralGenerator(minorModel, majorModel, attempts))

  def choralGenerator(minorModel: Model, majorModel: Model, attempts: Int): HttpApp[IO] =
    HttpRoutes.of[IO] {
      case request@GET -> Root / "choral" / inputKey / inputMode =>
        val key = Try(Note.valueOf(inputKey.trim)).getOrElse(Note.c)
        val mode: Mode = if inputMode.trim.equals("major") then Mode.maj else Mode.min
        val model = if mode equals Mode.maj then majorModel else minorModel
        val pdfId = r.nextInt.toString
        val midId = r.nextInt.toString
        val lilypondToPdfPath = s"${outputPath}choral$pdfId.ly"
        val lilypondToMidiPath = s"${outputPath}choral$midId.ly"
        val generation = Try {
          val choral = model.generateChoral(r, key, mode)
          val harmonizedChoral = harmonizeChoral(choral)
          writeTextToFile(lilypondToPdfPath, harmonizedChoral.toLilypondFileFormat(false))
          writeTextToFile(lilypondToMidiPath, harmonizedChoral.toLilypondFileFormat(true))
          s"lilypond -fpdf $lilypondToPdfPath $lilypondToMidiPath".!!
        }
        generation match {
          case Success(_) =>  Ok (FileUrls (pdf = pdfId, midi = midId))
          case Failure(ex) =>
            if attempts > 0 then choralGenerator(minorModel, majorModel, attempts - 1)(request)
            else InternalServerError(s"Unable to generated choral: $ex")
        }

      case request@GET -> Root / "midi" / fileId =>
        StaticFile.fromPath(Path(s"choral$fileId.midi"), Some(request)).getOrElseF(NotFound())

      case request@GET -> Root / "pdf" / fileId =>
        StaticFile.fromPath(Path(s"choral$fileId.pdf"), Some(request)).getOrElseF(NotFound())

      case request@GET -> Root / "badge" => Ok()
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
    port <- IO(Port.fromString(sys.env.getOrElse("PORT", throw new NoSuchElementException("Port env variable not found"))).get)
    minorModel <- readModelFromFile(s"${modelsPath}minor.model")
    majorModel <- readModelFromFile(s"${modelsPath}major.model")
    exit <- EmberServerBuilder
      .default[IO]
      .withHost(ipv4"0.0.0.0")
      .withPort(port)
      .withHttpApp(corsService(minorModel, majorModel, initialAttempts))
      .build
      .use(_ => IO.never)
      .as(ExitCode.Success)
  } yield exit
}

