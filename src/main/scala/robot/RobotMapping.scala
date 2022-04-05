package robot
import cats.implicits._
import Robot.{CustomFactor, Element, Event, EventsInfo, Factor, FactorsCatalog, Sport, Table}
import java.time.{Instant, LocalDateTime, ZoneId}

object RobotMapping {
  //{.league, .home, .away, .gameInfo (KO time), .ot(offer type = market name), .lineID, .oh, .oa, .od};

  type SportMap = Map[SportId, String]
  type EventMap = Map[EventId, EventData]

  case class EventData(league: String, home: String, away: String, gameInfo: LocalDateTime)
  case class SportId(value: Int)
  case class EventId(id: Int)
  case class FactorId(id: Int)

  sealed trait OddsKey

  object OddsKey {
    case object Home extends OddsKey
    case object Away extends OddsKey
    case object Draw extends OddsKey

    def oddsKeyString: OddsKey => String = {
      case Home => "1"
      case Away => "2"
      case Draw => "X"
    }

    def values: List[OddsKey] = List(Home, Away, Draw)
  }

  def getOh(factors: List[Factor], map: MarketsMap): Option[BigDecimal] = getOddsFromKey(factors, map, OddsKey.Home)
  def getOa(factors: List[Factor], map: MarketsMap): Option[BigDecimal] = getOddsFromKey(factors, map, OddsKey.Away)
  def getDraw(factors: List[Factor], map: MarketsMap): Option[BigDecimal] = getOddsFromKey(factors, map, OddsKey.Draw)

  def getLeague(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.league)
  def getHome(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.home)
  def getAway(eventId: EventId, eventMap: EventMap): Option[String] = eventMap.get(eventId).map(_.away)
  def getGameInfo(eventId: EventId, eventMap: EventMap): Option[LocalDateTime] = eventMap.get(eventId).map(_.gameInfo)

  def getOddsFromKey(factors: List[Factor], map: MarketsMap, key: OddsKey): Option[BigDecimal] =
  map.get(key).flatMap{
    case FactorId(id) => factors.collectFirst{
      case Factor(f, v, _) if f == id => v
    }
  }

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

  def getSportMap(sports: List[Sport]): SportMap = sports.map(sport => SportId(sport.id) -> sport.name).toMap

  def getEventsMap(eventsInfo: EventsInfo, sportMap: SportMap): EventMap = {
    for {
      event <- eventsInfo.events
      eventData <- getEventData(event, sportMap).toList
    } yield (EventId(event.id), eventData)
  }.toMap

  type MarketsMap = Map[OddsKey, FactorId]
  def getMarketsMap(factorsCatalog: FactorsCatalog): MarketsMap = {
    for {
      table <- resolveTables(factorsCatalog.groups.flatMap(_.tables)).iterator.find(table => table == Some("1x2"))
      values <- table match {
        case OneDTable(_, map) => OddsKey.values.traverse(oddsKey => map.get(OddsKey.oddsKeyString(oddsKey)).map(id => oddsKey -> FactorId(id)))
        case _ => None
      }
    } yield values.toMap
  }.getOrElse(Map.empty)

  def getEventData(event: Event, sportMap: SportMap): Option[EventData] = (sportMap.get(SportId(event.sportId)),event.team1,event.team2, Some(hongKongLocal(event.startTime))).mapN(EventData)

  case class OddsRow(league : String, home: String, away: String, gameInfo: LocalDateTime, ot: String, oh: BigDecimal, oa: BigDecimal, od: Option[BigDecimal])

  def resolveOddsRows(factors : List[CustomFactor], eventMap: EventMap, marketsMap : MarketsMap) : List[OddsRow] = for {
    factor <- factors
    oddsRow <- getOddsRows(factor, eventMap, marketsMap)
  } yield oddsRow

  sealed trait ResolvedTable
  case class OneDTable(map: Map[String, Int]) extends ResolvedTable
  case class TwoDTable(dimension: String, map: Map[(String, String), Int])

  case class NamedTable(name: Option[String], resolvedTable: ResolvedTable)

  def resolveHeader(row: List[Element]): Option[Header] = row.traverse{
    case Element.Label(name) => Some(name)
    case Element.Factor(_) => None
  }.map(Header)

  def resolveTables(tables: List[Table]) : List[NamedTable] = collectSomes(tables.map(resolveTable))

  def collectSomes[A](options: List[Option[A]]): List[A] = options.foldRight(List.empty[A]){
    case (Some(a), acc) => a::acc
    case (None, acc) => acc
  }

  def resolveTable(table: Table): Option[NamedTable] = table.rows match{
    case header :: rows => resolveHeader(header).flatMap(resolveRows(_, rows)).map(resolvedTable => NamedTable(table.name, resolvedTable))
    case Nil => None
  }
  case class Header(labels: List[String])
  def resolveRows(header: Header, rows: List[List[Element]]): Option[ResolvedTable] = resolveTwoDTable(header, rows) <+> resolveOneDTable(header, rows)

  def resolveTwoDTable(header: Header, value: List[List[Element]]): Option[ResolvedTable] = ??? //header.labels match {
 //   case zeroZeroElement :: labels => TwoDTable(???, zeroZeroElement, labels.traverse(label => ???))
 // }

  def resolveOneDTable(header: Header, value: List[List[Element]]): Option[ResolvedTable] = ???

  def hongKongLocal(milliseconds: Long): LocalDateTime = Instant.ofEpochMilli(milliseconds).atZone(ZoneId.of("Asia/Hong_Kong")).toLocalDateTime

}
