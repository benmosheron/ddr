
package ben

import scala.util.Random
import scala.math.Ordering

// Supported Expansion Packs
sealed trait Pack
object Pack { val all: Set[Pack] = Set(Base, Seaside, Prosperity) }
case object Base extends Pack
case object Seaside extends Pack
case object Prosperity extends Pack

// Extra resource required by some cards
sealed trait Resource { val name: String }
case object Curse extends Resource { val name = "Curse" }
case object EmbargoToken extends Resource { val name = "Embargo Token" }
case object CoinToken extends Resource { val name = "Coin Token" }
case object VictoryToken extends Resource { val name = "Victory Token" }
case object NativeVillageMat extends Resource { val name = "Native Village Mat" }
case object IslandMat extends Resource { val name = "Island Mat" }
case object PirateShipMat extends Resource { val name = "Pirate Ship Mat" }
case object TradeRouteMat extends Resource { val name = "Trade Route Mat" }

// Cost to buy the card.
sealed trait Cost
object Cost {
  implicit object CostOrdering extends Ordering[Cost] {
    def compare(a: Cost, b: Cost): Int = (a,b) match {
      case (MiscCost, MiscCost)         => 0
      case (MiscCost, _)                => 1
      case (_, MiscCost)                => -1
      case (KnownCost(i), KnownCost(j)) => implicitly[Ordering[Int]].compare(i,j)
    }
  }
}
case object MiscCost extends Cost
case class KnownCost(v: Int) extends Cost

case class Card(name: String, pack: Pack, resources: Set[Resource], cost: Cost)
object Card {
  implicit object CardOrderingByCost extends Ordering[Card] {
    def compare(a: Card, b: Card): Int = Cost.CostOrdering.compare(a.cost, b.cost)
  }
  implicit val CardOrderingByName: Ordering[Card] = Ordering.by[Card, String](_.name)
}


object App {

  import Card.CardOrderingByName

  type Deck = Set[Card]

  val random = new Random()

  val packName: Pack => String = p => p.toString.toLowerCase

  def parsePacks(packStrings: List[String]): Set[Pack] = if (packStrings.isEmpty) Pack.all else {
    val packsStringsLC = packStrings.map(_.toLowerCase)
    // Print unknown packs
    packsStringsLC
      .foreach(p => if(!Pack.all.map(packName).contains(p)) println(s"Unknown pack [$p]"))
    Pack.all.filter(p => packsStringsLC.contains(packName(p)))
  }

  def resources(deck: Deck): Set[Resource] = deck.flatMap(_.resources)

  // Pick a random set of cards
  def pick(fromPacks: Set[Pack])(all: Deck): Deck = {
    val n = 10
    val domain = all.filter(c => fromPacks.contains(c.pack))
    def _pick(remaining: Deck, current: Deck): Deck = {
      if     (current.size == n) current
      else if(remaining.size == 0) current
      else {
        val next = remaining.maxBy(_ => random.nextInt)
        _pick(remaining - next, current + next)
      }
    }
    _pick(domain, Set())
  }

  def printCost(cost: Cost): String = cost match {
    case MiscCost => "U"
    case KnownCost(c) => c.toString
  }

  def printCards(cards: List[Card]): String = cards.sorted.map(c => "  " + c.name + " (" + printCost(c.cost) + ")").mkString("\n")

  def printResources(deck: Deck): String = {
    val resourcesNeeded = resources(deck)
    if(resourcesNeeded.isEmpty) "You don't need any resources"
    else s"""
      |Resources you will need:
      |${resourcesNeeded.map(_.name).map("  " + _).mkString("\n")}
      """.stripMargin
  }

  def printDeck(deck: Deck): String = deck.groupBy(_.pack).map {
    case (pack, cards) => s"""
    |$pack (${cards.size}):
    |${printCards(cards.toList)}
    """.stripMargin
  }.mkString("\n") + "\n" + printResources(deck)

  def main(args: Array[String]): Unit = {
    val packs = parsePacks(args.toList)
    println(printDeck(pick(packs)(Cards.cards)))
  }
}