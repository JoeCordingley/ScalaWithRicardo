package robot

import cats.effect.{IO, IOApp, ExitCode}
import cats.implicits._
import org.http4s.implicits._
import org.http4s.{Uri, Request, Method, Headers, Header}
import scala.concurrent.ExecutionContext.global
import org.http4s.blaze.client._
import io.circe.Json
import org.http4s.circe._
import org.typelevel.ci._
import org.http4s.client.middleware.GZip

object RobotAlpha extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = BlazeClientBuilder[IO].resource.map(GZip.apply[IO]()).use { client =>
    client.expect[Json](Request[IO](method = Method.GET, uri = factorsMapping))
  }.flatMap(IO.println).map(_ => ExitCode.Success)

  
  val factorsMapping: Uri =uri"https://line32.bkfon-resources.com/line/factorsCatalog/tables/?lang=en&version=0"
  val uri2: Uri = uri"https://line510.bkfon-resources.com/events/list?lang=en&scopeMarket=1600&version=0"


}
