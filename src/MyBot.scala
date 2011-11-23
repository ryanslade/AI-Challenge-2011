import collection.mutable.{HashSet}

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  def ordersFrom(game: Game): Set[Order] = {

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values
    val occupiedTiles = new HashSet[Tile]

    // Add current ant postions to occupied tiles
    ants.foreach(a => occupiedTiles += a.tile)

    val result = ants.flatMap{ant =>
      var nextTile = game.tile(North).of(ant.tile) // HACK

      if (!game.board.enemyHills.isEmpty){
        val target = game.board.enemyHills.head._1
        val route = game.route(ant.tile, target)
        if (!route.isEmpty){
          nextTile = route.head
        }

      }
      else if (!game.board.food.isEmpty){
        val foodToEat = game.closestFood(ant)
        val route = game.route(ant.tile, foodToEat)
        if (!route.isEmpty){
          nextTile = route.head
        }
      }
      else{
        val direction = directions.find{aim =>
          val targetTile = game.tile(aim).of(ant.tile)
          !game.board.water.contains(targetTile) && !occupiedTiles.contains(targetTile)
        }
        if (!direction.isEmpty){
          nextTile = game.tile(direction.head).of(ant.tile)
        }
      }

      val direction = directions.find{aim =>
        !occupiedTiles.contains(nextTile) && game.tile(aim).of(ant.tile) == nextTile
      }

      // convert this (possible) direction into an order for this ant
      val order = direction.map(d => Order(ant.tile, d))

      if (!direction.isEmpty){
        // Add new position and remove old
        occupiedTiles += game.tile(direction.head).of(ant.tile)
        occupiedTiles -= ant.tile
      }

      order
    }.toSet

    result
  }
}
