package ben

object Cards {

  type ProtoCard = (String, Int, Set[Resource])

  def toCard(pack: Pack)(c: (String, Int, Set[Resource])): Card = c match {
    case (name, -1, r)   => Card(name, pack, r, MiscCost)
    case (name, cost, r) => Card(name, pack, r, KnownCost(cost))
  }

  val base = Set(
    ("Cellar",       2, Set[Resource]()),
    ("Chapel",       2, Set[Resource]()),
    ("Moat",         2, Set[Resource]()),
    ("Chancellor",   3, Set[Resource]()),
    ("Village",      3, Set[Resource]()),
    ("Woodcutter",   3, Set[Resource]()),
    ("Workshop",     3, Set[Resource]()),
    ("Bureaucrat",   4, Set[Resource]()),
    ("Feast",        4, Set[Resource]()),
    ("Gardens",      4, Set[Resource]()),
    ("Militia",      4, Set[Resource]()),
    ("Moneylender",  4, Set[Resource]()),
    ("Remodel",      4, Set[Resource]()),
    ("Smithy",       4, Set[Resource]()),
    ("Spy",          4, Set[Resource]()),
    ("Thief",        4, Set[Resource]()),
    ("Throne Room",  4, Set[Resource]()),
    ("Council Room", 5, Set[Resource]()),
    ("Festival",     5, Set[Resource]()),
    ("Laboratory",   5, Set[Resource]()),
    ("Library",      5, Set[Resource]()),
    ("Market",       5, Set[Resource]()),
    ("Mine",         5, Set[Resource]()),
    ("Witch",        5, Set[Resource](Curse)),
    ("Adventurer",   6, Set[Resource]())
  ).map(toCard(Base))

  val seaside = Set(
    ("Embargo",         2, Set[Resource](EmbargoToken)),
    ("Haven",           2, Set[Resource]()),
    ("Lighthouse",      2, Set[Resource]()),
    ("Native Village",  2, Set[Resource](NativeVillageMat)),
    ("Pearl Diver",     2, Set[Resource]()),
    ("Ambassador",      3, Set[Resource]()),
    ("Fishing Village", 3, Set[Resource]()),
    ("Lookout",         3, Set[Resource]()),
    ("Smugglers",       3, Set[Resource]()),
    ("Warehouse",       3, Set[Resource]()),
    ("Caravan",         4, Set[Resource]()),
    ("Cutpurse",        4, Set[Resource]()),
    ("Island",          4, Set[Resource](IslandMat)),
    ("Navigator",       4, Set[Resource]()),
    ("Pirate Ship",     4, Set[Resource](PirateShipMat, CoinToken)),
    ("Salvager",        4, Set[Resource]()),
    ("Sea Hag",         4, Set[Resource](Curse)),
    ("Treasure Map",    4, Set[Resource]()),
    ("Bazaar",          5, Set[Resource]()),
    ("Explorer",        5, Set[Resource]()),
    ("Ghost Ship",      5, Set[Resource]()),
    ("Merchant Ship",   5, Set[Resource]()),
    ("Outpost",         5, Set[Resource]()),
    ("Tactician",       5, Set[Resource]()),
    ("Treasury",        5, Set[Resource]()),
    ("Wharf",           5, Set[Resource]())
  ).map(toCard(Seaside))

  val prosperity = Set(
    ("Loan",             3, Set[Resource]()),
    ("Trade Route",      3, Set[Resource](TradeRouteMat, CoinToken)),
    ("Watchtower",       3, Set[Resource]()),
    ("Bishop",           4, Set[Resource](VictoryToken)),
    ("Monument",         4, Set[Resource](VictoryToken)),
    ("Quarry",           4, Set[Resource]()),
    ("Talisman",         4, Set[Resource]()),
    ("Worker's Village", 4, Set[Resource]()),
    ("City",             5, Set[Resource]()),
    ("Contraband",       5, Set[Resource]()),
    ("Counting House",   5, Set[Resource]()),
    ("Mint",             5, Set[Resource]()),
    ("Mountebank",       5, Set[Resource](Curse)),
    ("Rabble",           5, Set[Resource]()),
    ("Royal Seal",       5, Set[Resource]()),
    ("Vault",            6, Set[Resource]()),
    ("Venture",          6, Set[Resource]()),
    ("Goons",            6, Set[Resource](VictoryToken)),
    ("Grand Market",     6, Set[Resource]()),
    ("Hoard",            6, Set[Resource]()),
    ("Bank",             7, Set[Resource]()),
    ("Expand",           7, Set[Resource]()),
    ("Forge",            7, Set[Resource]()),
    ("King's Court",     7, Set[Resource]()),
    ("Peddler",          8, Set[Resource]())
  ).map(toCard(Prosperity))

  val renaissance = Set(
    ("Border Guard",     2,Set[Resource]()),
    ("Ducat",            2,Set[Resource]()),
    ("Lackeys",          2,Set[Resource]()),
    ("Acting Troupe",    3,Set[Resource]()),
    ("Cargo Ship",       3,Set[Resource]()),
    ("Experiment",       3,Set[Resource]()),
    ("Improve",          3,Set[Resource]()),
    ("Flag Bearer",      4,Set[Resource]()),
    ("Hideout",          4,Set[Resource]()),
    ("Inventor",         4,Set[Resource]()),
    ("Mountain Village", 4,Set[Resource]()),
    ("Patron",           4,Set[Resource]()),
    ("Priest",           4,Set[Resource]()),
    ("Research",         4,Set[Resource]()),
    ("Silk Merchant",    4,Set[Resource]()),
    ("Old Witch",        5,Set[Resource]()),
    ("Recruiter",        5,Set[Resource]()),
    ("Scepter",          5,Set[Resource]()),
    ("Scholar",          5,Set[Resource]()),
    ("Sculptor",         5,Set[Resource]()),
    ("Seer",             5,Set[Resource]()),
    ("Spices",           5,Set[Resource]()),
    ("Swashbuckler",     5,Set[Resource]()),
    ("Treasurer",        5,Set[Resource]()),
    ("Villain",          5,Set[Resource]())
    // ("Flag",             ,Set[Resource]()), // these are projects/artifacts
    // ("Horn",             ,Set[Resource]()),
    // ("Key",              ,Set[Resource]()),
    // ("Lantern",          ,Set[Resource]()),
    // ("Treasure Chest",   ,Set[Resource]()),
    // ("Cathedral",        ,Set[Resource]()),
    // ("City Gate",        ,Set[Resource]()),
    // ("Pageant",          ,Set[Resource]()),
    // ("Sewers",           ,Set[Resource]()),
    // ("Star Chart",       ,Set[Resource]()),
    // ("Exploration",      ,Set[Resource]()),
    // ("Fair",             ,Set[Resource]()),
    // ("Silos",            ,Set[Resource]()),
    // ("Sinister Plot",    ,Set[Resource]()),
    // ("Academy",          ,Set[Resource]()),
    // ("Capitalism",       ,Set[Resource]()),
    // ("Fleet",            ,Set[Resource]()),
    // ("Guildhall",        ,Set[Resource]()),
    // ("Piazza",           ,Set[Resource]()),
    // ("Road Network",     ,Set[Resource]()),
    // ("Barracks",         ,Set[Resource]()),
    // ("Crop Rotation",    ,Set[Resource]()),
    // ("Innovation",       ,Set[Resource]()),
    // ("Canal",            ,Set[Resource]()),
    // ("Citadel",          ,Set[Resource]())
  ).map(toCard(Renaissance))

  val cards: Set[Card] = base ++ seaside ++ prosperity ++ renaissance
}