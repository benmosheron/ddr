### Dominion Deck Randomiser

A command line tool for generating dominion games. Currently supports the following expansions:
* base game
* seaside
* prosperity
* renaissance
* menagerie

### Prerequisites

* scala programming language

### Installation

* verify you have scala installed. The commands `scala` and `scalac` are required.
* clone this repository
* on unix: source source-function.sh
* build with `build`
* run with `run`

### Example

Generate a random game for 4 players using cards from all supported expansions:

`run 4` 

Generate a random game using cards from the base game, seaside and prosperity:

`run 4 base seaside prosperity`

or

`run 4 b s p`
