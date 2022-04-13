package robot

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import robot.RobotMapping._
import robot.Robot._

class TableTest extends AnyFreeSpec with Matchers{
  "resolveTable" - {
    "should return a 1d table for 1X2" in {
      val table = Table(name = Some("1x2"), rows = List(
        List(Element.Label("1"), Element.Label("X"), Element.Label("2")),
        List(Element.Factor(921), Element.Factor(922), Element.Factor(923)),
      ))

      val resolvedTable = NamedTable(Some("1x2"), OneDTable(Map("1" -> 921, "X" -> 922, "2" -> 923)))

      resolveTable(table) shouldEqual Some(resolvedTable)
    }

    "should return a 2d table for Legs after" in {
      val table = Table(name = None, rows = List(
        List(Element.Label("Legs after"), Element.Label("%1"), Element.Label("Draw"), Element.Label("%2")),
        List(Element.Label("Score after %P legs"), Element.Factor(3738), Element.Factor(3739), Element.Factor(3740)),
        List(Element.Label("Score after %P legs"), Element.Factor(3742), Element.Factor(3743), Element.Factor(3744)),
        List(Element.Label("Score after %P legs"), Element.Factor(3746), Element.Factor(3747), Element.Factor(3748)),
        List(Element.Label("Score after %P legs"), Element.Factor(3750), Element.Factor(3751), Element.Factor(3752)),
        List(Element.Label("Score after %P legs"), Element.Factor(3754), Element.Factor(3755), Element.Factor(3756)),
      ))

      val resolvedTable = NamedTable(name = None, resolvedTable = TwoDTable(dimension = "Legs after", map = Map(
        ("Score after %1 legs", "%1") -> 3738,
        ("Score after %1 legs", "Draw") -> 3739,
        ("Score after %1 legs", "%2") -> 3740,
        ("Score after %2 legs", "%1") -> 3742,
        ("Score after %2 legs", "Draw") -> 3743,
        ("Score after %2 legs", "%2") -> 3744,
        ("Score after %3 legs", "%1") -> 3746,
        ("Score after %3 legs", "Draw") -> 3747,
        ("Score after %3 legs", "%2") -> 3748,
        ("Score after %4 legs", "%1") -> 3750,
        ("Score after %4 legs", "Draw") -> 3751,
        ("Score after %4 legs", "%2") -> 3752,
        ("Score after %5 legs", "%1") -> 3754,
        ("Score after %5 legs", "Draw") -> 3755,
        ("Score after %5 legs", "%2") -> 3756,
      )))

      resolveTable(table) shouldEqual Some(resolvedTable)
    }
  }


}
