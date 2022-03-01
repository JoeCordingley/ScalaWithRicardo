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

import java.time.LocalDateTime

object RobotAlpha extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = BlazeClientBuilder[IO].resource.map(GZip.apply[IO]()).use { client =>
    client.expect[Json](Request[IO](method = Method.GET, uri = uri2))
  }.flatMap(IO.println).map(_ => ExitCode.Success)

  val factorsMapping: Uri =uri"https://line32.bkfon-resources.com/line/factorsCatalog/tables/?lang=en&version=0"   //<- one where we'll build the market map from

  val uri2: Uri = uri"https://line510.bkfon-resources.com/events/list?lang=en&scopeMarket=1600&version=0"   //with all the events, odds and etc

  //"1x2-> (factor#ofHome)(option#ofDraw)(factor#ofaway)"
  //"moneyline 3way-> (factorofHome))(factorofaway)"

  //{.league, .home, .away, .gameInfo (KO time), .ot(offer type = market name), .lineID, .oh, .oa, .od};


  //arrays within the response from [URI2]
  //sport -> gives us sport/league id

  case class FactorsCatalog(groups: List[Group])
  object FactorsCatalog {
    implicit val factorsCatalogDecoder: Decoder[FactorsCatalog] = deriveDecoder
  }

  case class Group(name: String, tables: List[Table])
  object Group{
    implicit val groupDecoder: Decoder[Group] = deriveDecoder
  }

  case class Table(name: Option[String], rows: List[List[Element]])
  object Table {
    implicit val tableDecoder: Decoder[Table] = deriveDecoder
  }

  case class Factor(f: Int, v: BigDecimal, pt: Option[String])

  case class CustomFactor(e: Int, factors: List[Factor])

  //in the array [sports], get every element that has a field "parentId", and it's equal to "1"

  case class EventData(league: String, home: String, away: String, gameInfo: LocalDateTime)
  case class EventId(id: Int)
  case class OddsRow(league : String, home: String, away: String, gameInfo: LocalDateTime, ot: String, oh: BigDecimal, oa: BigDecimal, od: Option[BigDecimal])

  type EventMap = Map[EventId, EventData]
  type MarketsMap = Map[FactorId, String]
  def resolveFactors(factors : List[CustomFactor], eventMap: EventMap) : List[OddsRow] = for {
    factor <- factors
    oddsRow <- getOddsRow(factor, eventMap).toList
  } yield oddsRow

  def getOddsRow(factor: CustomFactor, eventMap: EventMap, marketsMap : ): Option[OddsRow] = (
    getLeague(EventId(factor.e), eventMap),
    getHome(EventId(factor.e), eventMap),
    getAway(EventId(factor.e), eventMap),
    getGameInfo(EventId(factor.e), eventMap),
    getOt(factor.factors, marketsMap),
    getOh,
    getOa,
    Some(getDraw)
    ).mapN(OddsRow.apply)

  //{921, 922, 923}  -> 1x2
  //we can assume as true = draw is either an odds value (3way) or a points value (2way)
  //home /away
  //home == odd == over
  //away == even == under

  def getLeague(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.league)

  def getHome(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.home)
  def getAway(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.away)
  def getGameInfo(eventId: EventId, eventMap: EventMap): Option[LocalDateTime] = eventMap.get(eventId).map(_.gameInfo)

  def getOt: Option[String] = ???

  def getOh: Option[BigDecimal] = ???
  def getOa: Option[BigDecimal] = ???
  def getDraw: Option[BigDecimal] = ???



  sealed trait Element
  object Element {
    case class Label(name: String) extends Element
    case class Factor(factorId: Int) extends Element

    val labelDecoder : Decoder[Element] = deriveDecoder[Label].widen
    val factorDecoder : Decoder[Element] = deriveDecoder[Factor].widen
    implicit val elementDecoder: Decoder[Element] = labelDecoder or factorDecoder

  }
}

