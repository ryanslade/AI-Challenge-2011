import collection.mutable.{HashSet}
import scala.math.max

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  def diffuse(game: Game, maxIterations: Int = 200, bailoutMilliseconds: Long = 5) = {
    val maxValue = 1000000
    var newMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)
    var oldMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)

    var bailout = false
    var averageTime: Long = 0
    var iterations: Long = 0
    val diffusionStartTime = System.currentTimeMillis()

    while (!bailout){
      newMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)

      (0 to game.parameters.rows-1).foreach{row =>
        (0 to game.parameters.columns-1).foreach{col =>
          val tile = Tile(row = row, column = col)

          if (game.board.water.contains(tile) || game.board.myAnts.contains(tile))
          {
            newMap(row)(col) = 0
          }
          else{
            if (game.board.food.contains(tile)){
              newMap(row)(col) = maxValue
            }
            else if(game.board.enemyHills.contains(tile)){
              newMap(row)(col) = maxValue/2
            }
            else if (game.visibility(row)(col) == 0){
              newMap(row)(col) = maxValue/3
            }
            else{
              // Do diffusal
              val northTile = game.tile(North).of(tile)
              val eastTile = game.tile(East).of(tile)
              val southTile = game.tile(South).of(tile)
              val westTile = game.tile(West).of(tile)

              val north = oldMap(northTile.row)(northTile.column)
              val east = oldMap(eastTile.row)(eastTile.column)
              val south = oldMap(southTile.row)(southTile.column)
              val west = oldMap(westTile.row)(westTile.column)

              val newValue = (0.25 * (north + east + south + west)).toInt
              newMap(tile.row)(tile.column) = newValue
            }
          }
        }
      }

      oldMap = newMap.clone()

      iterations += 1
      averageTime = max(1, (System.currentTimeMillis() - diffusionStartTime)/iterations)
      val timeLeftInTurn = game.parameters.turnTime - (System.currentTimeMillis() - game.turnStartTime)
      bailout = ((averageTime > (timeLeftInTurn-bailoutMilliseconds)) || iterations > maxIterations)
    }

    System.err.println("Managed to do: " + iterations + " iterations. (Average "+averageTime +")")
    
    oldMap
  }

  def ordersFrom(game: Game): Set[Order] = {
    val visTime = System.currentTimeMillis()
    game.setupVisibility
    System.err.println("Vis time: "+ (System.currentTimeMillis() - visTime).toString +"ms")

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values

    val occupiedTiles = new HashSet[Tile]
    ants.foreach(ant => occupiedTiles += ant.tile)

    val maxIterations = 300
    val diffusionMap = diffuse(game, maxIterations, 20)

    System.err.println("Time left before moving ants: " + (game.parameters.turnTime - (System.currentTimeMillis() - game.turnStartTime)).toString)
    val antTime = System.currentTimeMillis()
    val result = ants.flatMap{ant =>
      // Pick the direction with the highest food smell
      val neighbours = game.neighboursOf(ant.tile).sortBy(n => diffusionMap(n.row)(n.column)).reverse
      val nextTile = neighbours.find(n => !game.board.water.contains(n) && !occupiedTiles.contains(n) && !game.board.myHills.contains(n))

      val direction = directions.find{aim =>
        nextTile.nonEmpty && game.tile(aim).of(ant.tile) == nextTile.head
      }

      if (direction.nonEmpty){
        occupiedTiles -= ant.tile
        occupiedTiles += game.tile(direction.head).of(ant.tile)
      }

      // convert this (possible) direction into an order for this ant
      direction.map{d => Order(ant.tile, d)}
    }.toSet

    System.err.println("Ant movements: "+ (System.currentTimeMillis() - antTime).toString +"ms")

    result
  }
}
