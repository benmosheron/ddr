package ben

object Cards {

  type ProtoCard = (String, Int, Set[Resource])

  def toCard(pack: Pack)(c: ProtoCard): Card = c match {
    case (name, -1, r)   => NormalCard(name, pack, r, MiscCost)
    case (name, cost, r) => NormalCard(name, pack, r, KnownCost(cost))
  }

  def toProject(pack: Pack)(c: ProtoCard): Card = c match {
    case (name, cost, r) => Project(name, pack, r, KnownCost(cost))
  }

  val none = Set[Resource]()
  def some(r: Resource*) = r.toSet

  val base = Set(
    ("Cellar",       2, none),
    ("Chapel",       2, none),
    ("Moat",         2, none),
    ("Chancellor",   3, none),
    ("Village",      3, none),
    ("Woodcutter",   3, none),
    ("Workshop",     3, none),
    ("Bureaucrat",   4, none),
    ("Feast",        4, none),
    ("Gardens",      4, none),
    ("Militia",      4, none),
    ("Moneylender",  4, none),
    ("Remodel",      4, none),
    ("Smithy",       4, none),
    ("Spy",          4, none),
    ("Thief",        4, none),
    ("Throne Room",  4, none),
    ("Council Room", 5, none),
    ("Festival",     5, none),
    ("Laboratory",   5, none),
    ("Library",      5, none),
    ("Market",       5, none),
    ("Mine",         5, none),
    ("Witch",        5, some(Curse)),
    ("Adventurer",   6, none)
  ).map(toCard(Base))

  val seaside = Set(
    ("Embargo",         2, some(EmbargoToken)),
    ("Haven",           2, none),
    ("Lighthouse",      2, none),
    ("Native Village",  2, some(NativeVillageMat)),
    ("Pearl Diver",     2, none),
    ("Ambassador",      3, none),
    ("Fishing Village", 3, none),
    ("Lookout",         3, none),
    ("Smugglers",       3, none),
    ("Warehouse",       3, none),
    ("Caravan",         4, none),
    ("Cutpurse",        4, none),
    ("Island",          4, some(IslandMat)),
    ("Navigator",       4, none),
    ("Pirate Ship",     4, some(PirateShipMat, CoinToken)),
    ("Salvager",        4, none),
    ("Sea Hag",         4, some(Curse)),
    ("Treasure Map",    4, none),
    ("Bazaar",          5, none),
    ("Explorer",        5, none),
    ("Ghost Ship",      5, none),
    ("Merchant Ship",   5, none),
    ("Outpost",         5, none),
    ("Tactician",       5, none),
    ("Treasury",        5, none),
    ("Wharf",           5, none)
  ).map(toCard(Seaside))

  val prosperity = Set(
    ("Loan",             3, none),
    ("Trade Route",      3, some(TradeRouteMat, CoinToken)),
    ("Watchtower",       3, none),
    ("Bishop",           4, some(VictoryToken)),
    ("Monument",         4, some(VictoryToken)),
    ("Quarry",           4, none),
    ("Talisman",         4, none),
    ("Worker's Village", 4, none),
    ("City",             5, none),
    ("Contraband",       5, none),
    ("Counting House",   5, none),
    ("Mint",             5, none),
    ("Mountebank",       5, some(Curse)),
    ("Rabble",           5, none),
    ("Royal Seal",       5, none),
    ("Vault",            6, none),
    ("Venture",          6, none),
    ("Goons",            6, some(VictoryToken)),
    ("Grand Market",     6, none),
    ("Hoard",            6, none),
    ("Bank",             7, none),
    ("Expand",           7, none),
    ("Forge",            7, none),
    ("King's Court",     7, none),
    ("Peddler",          8, none)
  ).map(toCard(Prosperity))

  val renaissance = Set(
    ("Border Guard",     2, some(Lantern, Horn)),
    ("Ducat",            2, some(Coffers)),
    ("Lackeys",          2, some(Villagers)),
    ("Acting Troupe",    3, some(Villagers)),
    ("Cargo Ship",       3, none),
    ("Experiment",       3, none),
    ("Improve",          3, none),
    ("Flag Bearer",      4, some(Flag)),
    ("Hideout",          4, some(Curse)),
    ("Inventor",         4, none),
    ("Mountain Village", 4, none),
    ("Patron",           4, some(Coffers)),
    ("Priest",           4, none),
    ("Research",         4, none),
    ("Silk Merchant",    4, some(Coffers, Villagers)),
    ("Old Witch",        5, some(Curse)),
    ("Recruiter",        5, some(Villagers)),
    ("Scepter",          5, none),
    ("Scholar",          5, none),
    ("Sculptor",         5, some(Villagers)),
    ("Seer",             5, none),
    ("Spices",           5, some(Coffers)),
    ("Swashbuckler",     5, some(Coffers, TreasureChest)),
    ("Treasurer",        5, some(Key)),
    ("Villain",          5, some(Coffers))
  ).map(toCard(Renaissance))

  val projects = Set(
    ("Cathedral",        3, none),
    ("City Gate",        3, none),
    ("Pageant",          3, some(Coffers)),
    ("Sewers",           3, none),
    ("Star Chart",       3, none),
    ("Exploration",      4, some(Coffers, Villagers)),
    ("Fair",             4, none),
    ("Silos",            4, none),
    ("Sinister Plot",    4, none),
    ("Academy",          5, some(Villagers)),
    ("Capitalism",       5, none),
    ("Fleet",            5, none),
    ("Guildhall",        5, some(Coffers)),
    ("Piazza",           5, none),
    ("Road Network",     5, none),
    ("Barracks",         6, none),
    ("Crop Rotation",    6, none),
    ("Innovation",       6, none),
    ("Canal",            7, none),
    ("Citadel",          8, none)
  ).map(toProject(Renaissance))

  val cards: Set[Card] = base ++ seaside ++ prosperity ++ renaissance
}