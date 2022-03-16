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

  type EventMap = Map[EventId, EventData]
  type MarketsMap = Map[OddsKey, FactorId]
  type SportMap = Map[SportId, String]

  case class SportId(value: Int)

  def getSportMap(sports: List[Sport]): SportMap = sports.map(sport => SportId(sport.id) -> sport.name).toMap

  def getEventsMap(eventsInfo: EventsInfo, sportMap: SportMap): EventMap = {
    for {
      event <- eventsInfo.events
      eventData <- getEventData(event, sportMap).toList
    } yield (EventId(event.id), eventData)
  }.toMap


  def getEventData(event: Event, sportMap: SportMap): Option[EventData] = (sportMap.get(SportId(event.sportId)),event.team1,event.team2, Some(hongKongLocal(event.startTime))).mapN(EventData)

  def hongKongLocal(milliseconds: Long): LocalDateTime = Instant.ofEpochMilli(milliseconds).atZone(ZoneId.of("Asia/Hong_Kong")).toLocalDateTime





  //{.league, .home, .away, .gameInfo (KO time), .ot(offer type = market name), .lineID, .oh, .oa, .od};

  case class Sport(id: Int, name: String)
  case class EventData(league: String, home: String, away: String, gameInfo: LocalDateTime)
  case class EventId(id: Int)
  case class FactorId(id: Int)
  case class OddsRow(league : String, home: String, away: String, gameInfo: LocalDateTime, ot: String, oh: BigDecimal, oa: BigDecimal, od: Option[BigDecimal])

  type EventMap = Map[EventId, EventData]
  type MarketsMap = Map[OddsKey, FactorId]

  def resolveFactors(factors : List[CustomFactor], eventMap: EventMap, marketsMap : MarketsMap) : List[OddsRow] = for {
    factor <- factors
    oddsRow <- getOddsRows(factor, eventMap, marketsMap)
  } yield oddsRow

  def getMoneyline(factor: CustomFactor, eventMap: EventMap, marketsMap : MarketsMap): Option[OddsRow] = (
    getLeague(EventId(factor.e), eventMap),
    getHome(EventId(factor.e), eventMap),
    getAway(EventId(factor.e), eventMap),
    getGameInfo(EventId(factor.e), eventMap),
    Some("1x2"),
    getOh(factor.factors, marketsMap),
    getOa(factor.factors, marketsMap),
    Some(getDraw(factor.factors, marketsMap))
    ).mapN(OddsRow.apply)

  def getOddsRows(factor: CustomFactor, eventMap: EventMap, marketsMap: MarketsMap) : List[OddsRow] = getMoneyline(factor, eventMap, marketsMap).toList

  //{921, 922, 923}  -> 1x2
  //("1x2" -> {921, 922, 923})
  //("Yes/no" -> {921, 922})
  /*
  HomeOdds, AwayOdds, DrawOdds              <-Moneyline 1x2
  HomeOdds, AwayOdds, Points                <-Handicap
  CorrectScoreHome, CorrectScoreAway, Odds  <-Correct Score
  OverOdds, UnderOdds, Points
  OddOdds, EvenOdds, Points
  YesOdds, NoOdds, Points                   <-Props

  [USPGAChampionship, 19357516, 2022-05-19 19:00, , HenrikStenson, Outright, 151.00, 0], <- this is golf, BUT it's a HorseRow
   */

  def getLeague(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.league)

  def getHome(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.home)
  def getAway(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.away)
  def getGameInfo(eventId: EventId, eventMap: EventMap): Option[LocalDateTime] = eventMap.get(eventId).map(_.gameInfo)

//  def getOt: Option[String] =
// Map[OddsKey, FactorId]
  def getOddsFromKey(factors: List[Factor], map: MarketsMap, key: OddsKey): Option[BigDecimal] =
    map.get(key).flatMap{
      case FactorId(id) => factors.collectFirst{
        case Factor(f, v, _) if f == id => v
      }
    }

  def getOh(factors: List[Factor], map: MarketsMap): Option[BigDecimal] = getOddsFromKey(factors, map, Home)

  def getOa(factors: List[Factor], map: MarketsMap): Option[BigDecimal] = getOddsFromKey(factors, map, Away)

  def getDraw(factors: List[Factor], map: MarketsMap): Option[BigDecimal] = getOddsFromKey(factors, map, Draw)

  sealed trait OddsKey
  case object Home extends OddsKey
  case object Away extends OddsKey
  case object Draw extends OddsKey

  sealed trait Element
  object Element {
    case class Label(name: String) extends Element
    case class Factor(factorId: Int) extends Element

    val labelDecoder : Decoder[Element] = deriveDecoder[Label].widen
    val factorDecoder : Decoder[Element] = deriveDecoder[Factor].widen
    implicit val elementDecoder: Decoder[Element] = labelDecoder or factorDecoder

  }

  sealed trait ResolvedTable
  case class OneDTable(name: String, map: Map[String, Int]) extends ResolvedTable
  case class TwoDTable(name: String, dimension: String, map: Map[(String, String), Int])

  def resolveTable(table: Table): Option[ResolvedTable] = table.rows match{
    case header :: rows => resolveHeader(header).flatMap(resolveRows(_, rows))
    case Nil => None
  }
  def resolveHeader(row: List[Element]): Option[Header] = row.traverse{
    case Element.Label(name) => Some(name)
    case Element.Factor(_) => None
  }.map(Header)

  case class Header(labels: List[String])
  def resolveRows(header: Header, rows: List[List[Element]]): Option[ResolvedTable] = resolveTwoDTable(header, rows) <+> resolveOneDTable(header, rows)

  def resolveTwoDTable(header: Header, value: List[List[Element]]): Option[ResolvedTable] = header.labels match {
    case zeroZeroElement :: labels => TwoDTable(???, zeroZeroElement, labels.traverse(label => ???))
  }

  def resolveOneDTable(header: Header, value: List[List[Element]]): Option[ResolvedTable] = ???


  def getMarketsMap(factorsCatalog: FactorsCatalog): MarketsMap = factorsCatalog.

  case class Event(id: Int, sportId: Int, team1: Option[String] , team2: Option[String] , startTime: Long)

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
  object Factor {
    implicit val factorDecoder: Decoder[Factor] = deriveDecoder
  }

  case class CustomFactor(e: Int, factors: List[Factor])
  object CustomFactor {
    implicit val customFactorDecoder: Decoder[CustomFactor] = deriveDecoder
  }


  object Event {
    implicit val eventDecoder: Decoder[Event] = deriveDecoder
  }
}

