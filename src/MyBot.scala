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
      var nextTile = game.tile(North).of(ant.tile) // HACK

      if (!game.board.enemyHills.isEmpty){
        val target = game.board.enemyHills.head._1
        val route = game.route(ant.tile, target, occupiedTiles)
        if (!route.isEmpty){
          nextTile = route.head
        }

      }
      else if (!game.board.food.isEmpty){
        val foodToEat = game.board.food.head._1
        val route = game.route(ant.tile, foodToEat, occupiedTiles)
        if (!route.isEmpty){
          nextTile = route.head
        }
      }
      else{
        val direction = directions.find{aim =>
          val targetTile = game.tile(aim).of(ant.tile)
          !game.board.water.contains(targetTile) && !occupiedTiles.contains(targetTile)
          game.tile(aim).of(ant.tile) == nextTile
        }.get
        nextTile = game.tile(direction).of(ant.tile)
      }

      val direction = directions.find{aim =>
        game.tile(aim).of(ant.tile) == nextTile
      }

      // convert this (possible) direction into an order for this ant
      val order = direction.map{d => Order(ant.tile, d)}

      if (direction != Nil){
        occupiedTiles += game.tile(direction.get).of(ant.tile)
      }

      order
    }.toSet
  }
}
