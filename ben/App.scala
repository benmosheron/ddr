
package ben

import scala.util.Random
import scala.math.Ordering

// Supported Expansion Packs
sealed trait Pack { val order: Int; val alias: String }
object Pack {
  val all: Set[Pack] = Set(Base, Seaside, Prosperity, Renaissance)
  val aliases: Set[String] = all.map(_.alias)
}
case object Base extends Pack { val order = 1; val alias = "b" }
case object Seaside extends Pack { val order = 2; val alias = "s" }
case object Prosperity extends Pack { val order = 3; val alias = "p" }
case object Renaissance extends Pack { val order = 4; val alias = "r" }

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
case object Lantern extends Resource { val name = "Lantern" }
case object Horn extends Resource { val name = "Horn" }
case object Coffers extends Resource { val name = "Coffers" }
case object Villagers extends Resource { val name = "Villagers" }
case object Flag extends Resource { val name = "Flag" }
case object TreasureChest extends Resource { val name = "Treasure Chest" }
case object Key extends Resource { val name = "Key" }

// Cost to buy the card.
sealed trait Cost
object Cost {
  implicit object CostOrdering extends Ordering[Cost] {
    def compare(a: Cost, b: Cost): Int = (a, b) match {
      case (MiscCost, MiscCost)         => 0
      case (MiscCost, _)                => 1
      case (_, MiscCost)                => -1
      case (KnownCost(i), KnownCost(j)) => implicitly[Ordering[Int]].compare(i,j)
    }
  }
}
case object MiscCost extends Cost
case class KnownCost(v: Int) extends Cost

sealed trait Card {
  val name: String
  val pack: Pack
  val resources: Set[Resource]
  val cost: Cost
}
object Card {
  implicit object CardOrderingByCost extends Ordering[Card] {
    def compare(a: Card, b: Card): Int = Cost.CostOrdering.compare(a.cost, b.cost)
  }
  implicit val CardOrderingByName: Ordering[Card] = Ordering.by[Card, String](_.name)
}
case class NormalCard(name: String, pack: Pack, resources: Set[Resource], cost: Cost) extends Card
case class Project(name: String, pack: Pack, resources: Set[Resource], cost: Cost) extends Card


object App {

  import Card.CardOrderingByName

  type Deck = Set[Card]

  val random = new Random()

  val packName: Pack => String = p => p.toString.toLowerCase

  def fst[A,B](t: (A, B)): A = t._1
  def snd[A,B](t: (A, B)): B = t._2

  // b r => base renaissance
  def deAliasPacks(packStrings: List[String]): List[String] = packStrings.map {
    case a if (Pack.aliases.contains(a)) => Pack.all.find(p => p.alias == a).get.toString
    case s => s
  }

  def parsePacks(packStrings: List[String]): Set[Pack] = if (packStrings.isEmpty) Pack.all else {
    val packsStringsLC = deAliasPacks(packStrings).map(_.toLowerCase)

    // Print unknown packs
    packsStringsLC
      .foreach(p => if(!Pack.all.map(packName).contains(p)) println(s"Unknown pack [$p]"))
    Pack.all.filter(p => packsStringsLC.contains(packName(p)))
  }

  def resources(deck: Deck): Set[Resource] = deck.flatMap(_.resources)

  // Pick a random set of cards
  def pickNFromDeck(n: Int, deck: Deck): Deck = {
    def _pick(remaining: Deck, current: Deck): Deck = {
      if     (current.size == n) current
      else if(remaining.size == 0) current
      else {
        val next = remaining.maxBy(_ => random.nextInt)
        _pick(remaining - next, current + next)
      }
    }
    _pick(deck, Set())
  }

  def pick(fromPacks: Set[Pack])(all: Deck)(projects: Deck): Deck = {
    val n = 10
    val domain = all.filter(c => fromPacks.contains(c.pack))

    val pickedCards = pickNFromDeck(n, domain)
    val pickedProjects = if (fromPacks.contains(Renaissance)) pickNFromDeck(2, projects) else Set()

    pickedCards ++ pickedProjects
  }

  def printCost(cost: Cost): String = cost match {
    case MiscCost => "U"
    case KnownCost(c) => c.toString
  }

  def printCard(card: Card): String = card match {
    case NormalCard(name, _, _, cost) => s"  $name (${printCost(cost)})"
    case Project(name, _, _, cost) => s"  $name (${printCost(cost)} Project)"
  }

  def printCards(cards: List[Card]): String = cards.sorted.map(printCard).mkString("\n")

  def printResources(deck: Deck): String = {
    val resourcesNeeded = resources(deck)
    if(resourcesNeeded.isEmpty) "You don't need any resources"
    else s"""
      |Resources you will need:
      |${resourcesNeeded.map(_.name).map("  " + _).mkString("\n")}
      """.stripMargin
  }

  def printDeck(deck: Deck): String = deck.groupBy(_.pack).map {
    case (pack, cards) => (s"""
    |$pack (${cards.size}):
    |${printCards(cards.toList)}
    """.stripMargin, pack.order)
  }
  .toList
  .sortBy(snd)
  .map(fst)
  .mkString("\n") + "\n" + printResources(deck)

  def main(args: Array[String]): Unit = {
    val packs = parsePacks(args.toList)
    println(printDeck(pick(packs)(Cards.cards)(Cards.projects)))
  }
}
