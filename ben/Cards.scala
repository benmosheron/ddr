package ben

object Cards {

  type ProtoCard = (String, Int, Set[Resource], Supply)

  def toCard(pack: Pack)(c: ProtoCard): Card = c match {
    case (name, -1, r, s)   => NormalCard(name, pack, r, MiscCost, s)
    case (name, cost, r, s) => NormalCard(name, pack, r, KnownCost(cost), s)
  }

  def toProject(pack: Pack)(c: (String, Int, Set[Resource])): Card = c match {
    case (name, cost, r) => Project(name, pack, r, KnownCost(cost))
  }

  def toEvent(pack: Pack)(c: (String, Int, Set[Resource])): Card = c match {
    case (name, cost, r) => Event(name, pack, r, KnownCost(cost))
  }

  def toWay(pack: Pack)(c: (String, Set[Resource])): Card = c match {
    case (name, r) => Way(name, pack, r, MiscCost)
  }

  val none = Set[Resource]()
  def some(r: Resource*) = r.toSet

  val ten: Supply = Ten
  val victory: Supply = Victory

  val base = Set(
    ("Cellar",       2, none,        ten),
    ("Chapel",       2, none,        ten),
    ("Moat",         2, none,        ten),
    ("Chancellor",   3, none,        ten),
    ("Village",      3, none,        ten),
    ("Woodcutter",   3, none,        ten),
    ("Workshop",     3, none,        ten),
    ("Bureaucrat",   4, none,        ten),
    ("Feast",        4, none,        ten),
    ("Gardens",      4, none,        victory),
    ("Militia",      4, none,        ten),
    ("Moneylender",  4, none,        ten),
    ("Remodel",      4, none,        ten),
    ("Smithy",       4, none,        ten),
    ("Spy",          4, none,        ten),
    ("Thief",        4, none,        ten),
    ("Throne Room",  4, none,        ten),
    ("Council Room", 5, none,        ten),
    ("Festival",     5, none,        ten),
    ("Laboratory",   5, none,        ten),
    ("Library",      5, none,        ten),
    ("Market",       5, none,        ten),
    ("Mine",         5, none,        ten),
    ("Witch",        5, some(Curse), ten),
    ("Adventurer",   6, none,        ten)
  ).map(toCard(Base))

  val seaside = Set(
    ("Embargo",         2, some(EmbargoToken),             ten),
    ("Haven",           2, none,                           ten),
    ("Lighthouse",      2, none,                           ten),
    ("Native Village",  2, some(NativeVillageMat),         ten),
    ("Pearl Diver",     2, none,                           ten),
    ("Ambassador",      3, none,                           ten),
    ("Fishing Village", 3, none,                           ten),
    ("Lookout",         3, none,                           ten),
    ("Smugglers",       3, none,                           ten),
    ("Warehouse",       3, none,                           ten),
    ("Caravan",         4, none,                           ten),
    ("Cutpurse",        4, none,                           ten),
    ("Island",          4, some(IslandMat),                victory),
    ("Navigator",       4, none,                           ten),
    ("Pirate Ship",     4, some(PirateShipMat, CoinToken), ten),
    ("Salvager",        4, none,                           ten),
    ("Sea Hag",         4, some(Curse),                    ten),
    ("Treasure Map",    4, none,                           ten),
    ("Bazaar",          5, none,                           ten),
    ("Explorer",        5, none,                           ten),
    ("Ghost Ship",      5, none,                           ten),
    ("Merchant Ship",   5, none,                           ten),
    ("Outpost",         5, none,                           ten),
    ("Tactician",       5, none,                           ten),
    ("Treasury",        5, none,                           ten),
    ("Wharf",           5, none,                           ten)
  ).map(toCard(Seaside))

  val prosperity = Set(
    ("Loan",             3, none,                           ten),
    ("Trade Route",      3, some(TradeRouteMat, CoinToken), ten),
    ("Watchtower",       3, none,                           ten),
    ("Bishop",           4, some(VictoryToken),             ten),
    ("Monument",         4, some(VictoryToken),             ten),
    ("Quarry",           4, none,                           ten),
    ("Talisman",         4, none,                           ten),
    ("Worker's Village", 4, none,                           ten),
    ("City",             5, none,                           ten),
    ("Contraband",       5, none,                           ten),
    ("Counting House",   5, none,                           ten),
    ("Mint",             5, none,                           ten),
    ("Mountebank",       5, some(Curse),                    ten),
    ("Rabble",           5, none,                           ten),
    ("Royal Seal",       5, none,                           ten),
    ("Vault",            6, none,                           ten),
    ("Venture",          6, none,                           ten),
    ("Goons",            6, some(VictoryToken),             ten),
    ("Grand Market",     6, none,                           ten),
    ("Hoard",            6, none,                           ten),
    ("Bank",             7, none,                           ten),
    ("Expand",           7, none,                           ten),
    ("Forge",            7, none,                           ten),
    ("King's Court",     7, none,                           ten),
    ("Peddler",          8, none,                           ten)
  ).map(toCard(Prosperity))

  val renaissance = Set(
    ("Border Guard",     2, some(Lantern, Horn),          ten),
    ("Ducat",            2, some(Coffers),                ten),
    ("Lackeys",          2, some(Villagers),              ten),
    ("Acting Troupe",    3, some(Villagers),              ten),
    ("Cargo Ship",       3, none,                         ten),
    ("Experiment",       3, none,                         ten),
    ("Improve",          3, none,                         ten),
    ("Flag Bearer",      4, some(Flag),                   ten),
    ("Hideout",          4, some(Curse),                  ten),
    ("Inventor",         4, none,                         ten),
    ("Mountain Village", 4, none,                         ten),
    ("Patron",           4, some(Coffers),                ten),
    ("Priest",           4, none,                         ten),
    ("Research",         4, none,                         ten),
    ("Silk Merchant",    4, some(Coffers, Villagers),     ten),
    ("Old Witch",        5, some(Curse),                  ten),
    ("Recruiter",        5, some(Villagers),              ten),
    ("Scepter",          5, none,                         ten),
    ("Scholar",          5, none,                         ten),
    ("Sculptor",         5, some(Villagers),              ten),
    ("Seer",             5, none,                         ten),
    ("Spices",           5, some(Coffers),                ten),
    ("Swashbuckler",     5, some(Coffers, TreasureChest), ten),
    ("Treasurer",        5, some(Key),                    ten),
    ("Villain",          5, some(Coffers),                ten)
  ).map(toCard(Renaissance))

  val menagerie = Set(
    ("Black Cat",     2, some(Curse),              ten),
    ("Sleigh",        2, some(Horse),              ten),
    ("Supplies",      2, some(Horse),              ten),
    ("Camel Train",   3, some(ExileMat),           ten),
    ("Goatherd",      3, none,                     ten),
    ("Scrap",         3, some(Horse),              ten),
    ("Sheepdog",      3, none,                     ten),
    ("Snowy Village", 3, none,                     ten),
    ("Stockpile",     3, some(ExileMat),           ten),
    ("Bounty Hunter", 4, some(ExileMat),           ten),
    ("Cardinal",      4, some(ExileMat),           ten),
    ("Cavalry",       4, some(Horse),              ten),
    ("Groom",         4, some(Horse),              ten),
    ("Hostelry",      4, some(Horse),              ten),
    ("Village Green", 4, none,                     ten),
    ("Barge",         5, none,                     ten),
    ("Coven",         5, some(Curse, ExileMat),    ten),
    ("Displace",      5, some(ExileMat),           ten),
    ("Falconer",      5, none,                     ten),
    ("Gatekeeper",    5, some(ExileMat),           ten),
    ("Hunting Lodge", 5, none,                     ten),
    ("Kiln",          5, none,                     ten),
    ("Livery",        5, some(Horse),              ten),
    ("Mastermind",    5, none,                     ten),
    ("Paddock",       5, some(Horse),              ten),
    ("Sanctuary",     5, some(ExileMat),           ten),
    ("Fisherman",     5, none,                     ten),
    ("Destrier",      6, none,                     ten),
    ("Wayfarer",      6, none,                     ten),
    ("Animal Fair",   7, none,                     ten),
  ).map(toCard(Menagerie))

  private val projects = Set(
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

  private val events = Set(
    ("Delay",          0, none),
    ("Desperation",    0, some(Curse)),
    ("Gamble",         2, none),
    ("Pursue",         2, none),
    ("Ride",           2, some(Horse)),
    ("Toil",           2, none),
    ("Enhance",        3, none),
    ("March",          3, none),
    ("Transport",      3, some(ExileMat)),
    ("Banish",         4, some(ExileMat)),
    ("Bargain",        4, some(Horse)),
    ("Invest",         4, some(ExileMat)),
    ("Seize the Day",  4, none),
    ("Commerce",       5, none),
    ("Demand",         5, some(Horse)),
    ("Stampede",       5, some(Horse)),
    ("Reap",           7, none),
    ("Enclave",        8, some(ExileMat)),
    ("Alliance",      10, none),
    ("Populate",      10, none),
  ).map(toEvent(Menagerie))

  private val ways = Set(
    ("Way of the Butterfly", none),
    ("Way of the Camel", none),
    ("Way of the Chameleon", none),
    ("Way of the Frog", none),
    ("Way of the Goat", none),
    ("Way of the Horse", none),
    ("Way of the Mole", none),
    ("Way of the Monkey", none),
    ("Way of the Mouse", none),
    ("Way of the Mule", none),
    ("Way of the Otter", none),
    ("Way of the Owl", none),
    ("Way of the Ox", none),
    ("Way of the Pig", none),
    ("Way of the Rat", none),
    ("Way of the Seal", none),
    ("Way of the Sheep", none),
    ("Way of the Squirrel", none),
    ("Way of the Turtle", none),
    ("Way of the Worm", none),
  ).map(toWay(Menagerie))

  val cards: Set[Card] = base ++ seaside ++ prosperity ++ renaissance ++ menagerie
  val extra: Set[Card] = projects ++ events ++ ways
}