package robot

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import io.circe.{Decoder, Json}
import org.http4s.blaze.client._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.client.middleware.GZip
import org.http4s.implicits._
import org.http4s.{Method, Request, Uri}
import io.circe.generic.semiauto._

import java.time.{Instant, LocalDateTime, ZoneId}

object Robot extends IOApp {
  val factorsMappingUri: Uri =uri"https://line32.bkfon-resources.com/line/factorsCatalog/tables/?lang=en&version=0"   //<- one where we'll build the market map from
  val eventsInfoUri: Uri = uri"https://line510.bkfon-resources.com/events/list?lang=en&scopeMarket=1600&version=0"   //with all the events, odds and etc

  override def run(args: List[String]): IO[ExitCode] = BlazeClientBuilder[IO].resource.map(GZip.apply[IO]()).use { client =>
    //client.expect[EventsInfo](Request[IO](method = Method.GET, uri = eventsInfoUri))
    for{
      factorsMapping <- client.expect[FactorsCatalog](Request[IO](method = Method.GET, uri = factorsMappingUri))
      eventsInfo <-  client.expect[EventsInfo](Request[IO](method = Method.GET, uri = eventsInfoUri))
    } yield {
    //  val eventsMap = getEventsMap(eventsInfo)
    //  val marketsMap = getMarketsMap(factorsMapping)
    //  resolveFactors(eventsInfo.customFactors, eventsMap, marketsMap)
      ()
    }
  }.flatMap(IO.println).map(_ => ExitCode.Success)

  case class EventsInfo(customFactors: List[CustomFactor], events: List[Event], sports: List[Sport])
  object EventsInfo {
    implicit val eventsInfoDecoder: Decoder[EventsInfo] = deriveDecoder
  }

  case class Sport(id: Int, name: String)
  object Sport {
    implicit val sportDecoder: Decoder[Sport] = deriveDecoder
  }

  sealed trait Element
  object Element {
    case class Label(name: String) extends Element
    case class Factor(factorId: Int) extends Element

    val labelDecoder : Decoder[Element] = deriveDecoder[Label].widen
    val factorDecoder : Decoder[Element] = deriveDecoder[Factor].widen
    implicit val elementDecoder: Decoder[Element] = labelDecoder or factorDecoder
  }

  case class FactorsCatalog(groups: List[Group])
  object FactorsCatalog {
    implicit val factorsCatalogDecoder: Decoder[FactorsCatalog] = deriveDecoder
  }

  case class Group(tables: List[Table])
  object Group{
    implicit val groupDecoder: Decoder[Group] = deriveDecoder
  }

  case class Table(name: Option[String], rows: List[List[Element]])
  object Table {
    implicit val tableDecoder: Decoder[Table] = deriveDecoder
  }

  case class Factor(f: Int, v: BigDecimal, pt: Option[String])
  object Factor {
    implicit val factorDecoder: Decoder[Factor] = deriveDecoder
  }

  case class CustomFactor(e: Int, factors: List[Factor])
  object CustomFactor {
    implicit val customFactorDecoder: Decoder[CustomFactor] = deriveDecoder
  }

  case class Event(id: Int, sportId: Int, team1: Option[String] , team2: Option[String] , startTime: Long)
  object Event {
    implicit val eventDecoder: Decoder[Event] = deriveDecoder
  }
}

