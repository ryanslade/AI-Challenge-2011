import collection.mutable.{HashMap, HashSet}

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    // Your logic goes here.
    // for example ...

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values

    val occupiedTiles = new HashSet[Tile]

    ants.flatMap{ant =>
      // for this ant, find the first direction which is not water, if any
      val direction = directions.find{aim =>
        val targetTile = game.tile(aim).of(ant.tile)
        !game.board.water.contains(targetTile) && !occupiedTiles.contains(targetTile)
      }
      // convert this (possible) direction into an order for this ant
      val order = direction.map{d => Order(ant.tile, d)}

      if (direction != null){
        occupiedTiles += game.tile(direction.get).of(ant.tile)
      }

      order
    }.toSet
  }
}
