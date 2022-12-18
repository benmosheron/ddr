
package ben

import scala.util.Random
import scala.math.Ordering
import scala.util.Try

// Supported Expansion Packs
sealed trait Pack { val order: Int; val alias: String; val resources: Set[Resource] = Set() }
object Pack {
  val all: Set[Pack] = Set(Base, Seaside, Prosperity, Renaissance)
  val aliases: Set[String] = all.map(_.alias)
  implicit val PackOrdering: Ordering[Pack] = Ordering[Int].on(_.order)
}
case object Base extends Pack { val order = 1; val alias = "b" }
case object Seaside extends Pack { val order = 2; val alias = "s" }
case object Prosperity extends Pack { val order = 3; val alias = "p"; override val resources: Set[Resource] = Set(Platinum, Colony) }
case object Renaissance extends Pack { val order = 4; val alias = "r" }

// Extra resource required by some cards
sealed trait Resource { val name: String; val pack: Pack; val supply: Supply = Unlimited }
object Resource {
  implicit val ResourceOrdering: Ordering[Resource] = Ordering[(Pack, String)].on(r => (r.pack, r.name))
  val permanent = Set(Copper, Silver, Gold, Estate, Duchy, Province)
}
abstract class AbstractResource(override val name: String, override val pack: Pack, override val supply: Supply) extends Resource
case object Copper extends AbstractResource("Copper", Base, Unlimited)
case object Silver extends AbstractResource("Silver", Base, Unlimited)
case object Gold extends AbstractResource("Gold", Base, Unlimited)
case object Estate extends AbstractResource("Estate", Base, Victory)
case object Duchy extends AbstractResource("Duchy", Base, Victory)
case object Province extends AbstractResource("Province", Base, Victory)
case object Platinum extends AbstractResource ("Platinum", Prosperity, Twelve)
case object Colony extends AbstractResource ("Colony", Prosperity, Victory)
case object Curse extends AbstractResource ("Curse", Base, CurseSupply)
case object EmbargoToken extends AbstractResource ("Embargo Token", Prosperity, Unlimited)
case object CoinToken extends AbstractResource ("Coin Token", Prosperity, Unlimited)
case object VictoryToken extends AbstractResource ("Victory Token", Prosperity, Unlimited)
case object NativeVillageMat extends AbstractResource ("Native Village Mat", Seaside, Players)
case object IslandMat extends AbstractResource ("Island Mat", Seaside, Players)
case object PirateShipMat extends AbstractResource ("Pirate Ship Mat", Seaside, Players)
case object TradeRouteMat extends AbstractResource ("Trade Route Mat", Seaside, Players)
case object Lantern extends AbstractResource ("Lantern", Renaissance, One)
case object Horn extends AbstractResource ("Horn", Renaissance, One)
case object Coffers extends AbstractResource ("Coffers", Renaissance, Unlimited)
case object Villagers extends AbstractResource ("Villagers", Renaissance, Unlimited)
case object Flag extends AbstractResource ("Flag", Renaissance, One)
case object TreasureChest extends AbstractResource ("Treasure Chest", Renaissance, One)
case object Key extends AbstractResource ("Key", Renaissance, One)

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

// How may to put in the supply
sealed trait Supply { def get(nPlayers:Int): Option[Int] }
case object Unlimited extends Supply { def get(nPlayers: Int) = None }
case object One extends Supply { def get(nPlayers: Int) = Some(1) }
case object Ten extends Supply { def get(nPlayers: Int) = Some(10) }
case object Twelve extends Supply { def get(nPlayers: Int) = Some(12) }
case object Players extends Supply { def get(nPlayers: Int) = Some(nPlayers) }
case object Victory extends Supply { def get(nPlayers: Int) = if(nPlayers == 2) Some(8) else Some(12) }
case object CurseSupply extends Supply{ def get(nPlayers: Int) = Some(10 * (nPlayers - 1)) }

sealed trait Card {
  val name: String
  val pack: Pack
  val resources: Set[Resource]
  val cost: Cost
  val supply: Supply
}
object Card {
  implicit object CardOrderingByCost extends Ordering[Card] {
    def compare(a: Card, b: Card): Int = Cost.CostOrdering.compare(a.cost, b.cost)
  }
  implicit val CardOrderingByName: Ordering[Card] = Ordering.by[Card, String](_.name)
}
case class NormalCard(name: String, pack: Pack, resources: Set[Resource], cost: Cost, supply: Supply) extends Card
case class Project(name: String, pack: Pack, resources: Set[Resource], cost: Cost) extends Card { val supply: Supply = One }


object App {

  import Card.CardOrderingByName
  import Resource.ResourceOrdering

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

  def resources(deck: Deck): List[Resource] = (deck.flatMap(_.resources) ++ deck.flatMap(_.pack.resources) ++ Resource.permanent).toList

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

  def pick(fromPacks: Set[Pack], all: Deck, projects: Deck): Deck = {
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

  def printCard(nPlayers: Int)(card: Card): String = card match {
    case NormalCard(name, _, _, cost, Unlimited) => s"  $name"
    case NormalCard(name, _, _, cost, supply) => s"  $name x ${supply.get(nPlayers).get}"
    case p @ Project(name, _, _, cost) => s"  $name x ${p.supply.get(nPlayers).get} (Project)"
  }

  def printCards(cards: List[Card], nPlayers: Int): String = cards.sorted.map(printCard(nPlayers)).mkString("\n")

  def printResource(nPlayers: Int)(resource: Resource): String = resource.supply match {
    case Unlimited => s"${resource.pack}: ${resource.name}"
    case _ => s"${resource.pack}: ${resource.name} x ${resource.supply.get(nPlayers).get}"
  }

  def printResources(deck: Deck, nPlayers: Int): String = {
    val resourcesNeeded = resources(deck).sorted
    if(resourcesNeeded.isEmpty) "You don't need any resources"
    else s"""
      |Resources you will need:
      |${resourcesNeeded.map(printResource(nPlayers)).map("  " + _).mkString("\n")}
      """.stripMargin
  }

  def printDeck(deck: Deck, nPlayers: Int): String = deck.groupBy(_.pack).map {
    case (pack, cards) => (s"""
    |$pack (${cards.size}):
    |${printCards(cards.toList, nPlayers)}
    """.stripMargin, pack.order)
  }
  .toList
  .sortBy(snd)
  .map(fst)
  .mkString("\n") + "\n" + printResources(deck, nPlayers)

  def main(args: Array[String]): Unit = args.toList match {
    case nPlayersString :: packsStrings if Try(nPlayersString.toInt).isSuccess =>
      val nPlayers = nPlayersString.toInt
      val packs = parsePacks(packsStrings)
      println()
      println(s"Generating game for [${nPlayers}] players, chosen from the following packs:")
      packs.foreach(println)
      println()
      println(printDeck(pick(packs, Cards.cards, Cards.projects), nPlayers))
    case _ =>
      println("The first argument should be the number of players.")
      println("E.g.")
      println("run 4")
  }
}
