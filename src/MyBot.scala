import collection.mutable.{HashMap, HashSet}

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {

  def ordersFrom(game: Game): Set[Order] = {

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values

    val occupiedTiles = new HashSet[Tile]

    ants.flatMap{ant =>
      val foodToEat = game.board.food.head._1
      val nextTile = game.route(ant.tile, foodToEat, occupiedTiles).head

      val direction = directions.find{aim =>
        game.tile(aim).of(ant.tile) == nextTile
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
