import collection.mutable.{HashMap, HashSet}

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  def diffuse(game: Game, iterations: Int = 1) = {
    val maxValue = 1000000
    var newMap = getEmptyDiffusionMap
    var oldMap = getEmptyDiffusionMap

    for (i <- 1 to iterations){
      newMap.clear()

      (0 to game.parameters.rows).foreach{row =>
        (0 to game.parameters.columns).foreach{col =>
          val tile = Tile(row = row, column = col)

          if (game.board.water.contains(tile))
          {
            newMap += (row, col) -> 0
          }
          else{
            if (game.board.food.contains(tile)){
              newMap += (row, col) -> maxValue
            }
            else{
              // Do diffusal
              val northTile = game.tile(North).of(tile)
              val eastTile = game.tile(East).of(tile)
              val southTile = game.tile(South).of(tile)
              val westTile = game.tile(West).of(tile)

              val north = oldMap((northTile.row, northTile.column))
              val east = oldMap((eastTile.row, eastTile.column))
              val south = oldMap((southTile.row, southTile.column))
              val west = oldMap((westTile.row, westTile.column))

              val newValue = (0.25 * (north + east + south + west)).toInt
              newMap += (tile.row, tile.column) -> newValue
            }
          }

        }
      }

      oldMap = newMap.clone()
    }

    oldMap
  }

  def getEmptyDiffusionMap = {
    new HashMap[(Int, Int), Int]  { override def default(key:(Int, Int)) = 1 }
  }

  def ordersFrom(game: Game): Set[Order] = {

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values
    val occupiedTiles = new HashSet[Tile]

    val iterations = 20
    //val startTime = System.currentTimeMillis()
    val diffusionMap = diffuse(game, iterations)

    //System.err.println("Did "+iterations+" in "+ (System.currentTimeMillis()- startTime).toString +"ms")

    val result = ants.flatMap{ant =>
      // Pick the direction with the highest food smell
      val neighbours = game.neighboursOf(ant.tile).sortBy(n => diffusionMap((n.row, n.column))).reverse
      val nextTile = neighbours.find(n => !game.board.water.contains(n) && !occupiedTiles.contains(n))

      val direction = directions.find{aim =>
        nextTile.nonEmpty && game.tile(aim).of(ant.tile) == nextTile.head
      }

      if (!direction.isEmpty){
        // Add new position and remove old
        occupiedTiles += game.tile(direction.head).of(ant.tile)
      }

      // convert this (possible) direction into an order for this ant
      direction.map{d => Order(ant.tile, d)}
    }.toSet

    result
  }
}
