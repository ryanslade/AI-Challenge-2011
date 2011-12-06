import collection.mutable.{HashSet}
import scala.math.max

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  def diffuse(game: Game, maxIterations: Int = 200, bailoutMilliseconds: Long = 5) = {
    val maxValue = Int.MaxValue/4 // Need to divide by 4 since we are averaging lower down
    val minValue = -maxValue

    var newFoodMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)
    var oldFoodMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)

    var newExploreMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)
    var oldExploreMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)

    var bailout = false
    var averageTime: Long = 0
    var iterations: Long = 0
    val diffusionStartTime = System.currentTimeMillis()

    while (!bailout){
      newFoodMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)
      newExploreMap = Array.ofDim[Int](game.parameters.rows, game.parameters.columns)

      (0 to game.parameters.rows-1).foreach{row =>
        (0 to game.parameters.columns-1).foreach{col =>
          val tile = Tile(row = row, column = col)
          var doFood = true
          var doExplore = true

          if ((game.board.water.contains(tile) || game.board.myAnts.contains(tile)) && (!game.board.myHills.contains(tile)))
          {
            // Sometimes ants are on hills in which case we don't want to set it to minvalue
            newFoodMap(row)(col) = 0
            newExploreMap(row)(col) = 0
          }
          else{
            // Food and hills
            if (game.board.food.contains(tile)){
              newFoodMap(row)(col) = maxValue
              doFood = false
            }
            else if(game.board.enemyHills.contains(tile)){
              newFoodMap(row)(col) = maxValue/2
              doFood = false
            }

            if (game.visibility(row)(col) != game.turn){
              if (game.visibility(row)(col) == 0){
                newExploreMap(row)(col) = maxValue
              }
              else{
                newExploreMap(row)(col) = maxValue - 100*(game.parameters.turns - game.visibility(row)(col))
              }
              doExplore = false
            }

            if (doFood || doExplore){
              val northTile = game.tile(North).of(tile)
              val eastTile = game.tile(East).of(tile)
              val southTile = game.tile(South).of(tile)
              val westTile = game.tile(West).of(tile)

              if (doFood){
                // Diffuse food
                val northFood = oldFoodMap(northTile.row)(northTile.column)
                val eastFood = oldFoodMap(eastTile.row)(eastTile.column)
                val southFood = oldFoodMap(southTile.row)(southTile.column)
                val westFood = oldFoodMap(westTile.row)(westTile.column)

                val newValue = (0.25 * (northFood + eastFood + southFood + westFood)).toInt
                newFoodMap(tile.row)(tile.column) = newValue
              }

              if (doExplore){
                // Diffuse explore
                val northExplore = oldExploreMap(northTile.row)(northTile.column)
                val eastExplore = oldExploreMap(eastTile.row)(eastTile.column)
                val southExplore = oldExploreMap(southTile.row)(southTile.column)
                val westExplore = oldExploreMap(westTile.row)(westTile.column)

                val newValue = (0.25 * (northExplore + eastExplore + southExplore + westExplore)).toInt
                newExploreMap(tile.row)(tile.column) = newValue
              }
            }

          }
        }
      }

      oldFoodMap = newFoodMap.clone()
      oldExploreMap = newExploreMap.clone()

      iterations += 1
      averageTime = max(1, (System.currentTimeMillis() - diffusionStartTime)/iterations)
      val timeLeftInTurn = game.parameters.turnTime - (System.currentTimeMillis() - game.turnStartTime)
      bailout = ((averageTime > (timeLeftInTurn-bailoutMilliseconds)) || iterations > maxIterations)
    }

    System.err.println("Managed to do: " + iterations + " iterations. (Average "+averageTime +")")
    
    (oldFoodMap, oldExploreMap)
  }

  def ordersFrom(game: Game): Set[Order] = {

    def neighboursSortedByMap(neighbours: List[Tile],  diffusionMap: Array[Array[Int]], occupiedTiles: HashSet[Tile]) = {
      val sortedNeighbours = neighbours.sortBy(n => diffusionMap(n.row)(n.column)).reverse
      sortedNeighbours.find(n => !game.board.water.contains(n) && !occupiedTiles.contains(n) && !game.board.myHills.contains(n) && (diffusionMap(n.row)(n.column) != 0))
    }

    val visTime = System.currentTimeMillis()
    game.setupVisibility
    System.err.println("Vis time: "+ (System.currentTimeMillis() - visTime).toString +"ms")

    val directions = List(North, East, South, West)
    val ants = game.board.myAnts.values

    val occupiedTiles = new HashSet[Tile]
    ants.foreach(ant => occupiedTiles += ant.tile)

    val diffusionMaps = diffuse(game, 300, 20)

    System.err.println("Time left before moving ants: " + (game.parameters.turnTime - (System.currentTimeMillis() - game.turnStartTime)).toString)
    val antTime = System.currentTimeMillis()
    val result = ants.flatMap{ant =>
      val neighbours = game.neighboursOf(ant.tile)

      var nextTile = Option.empty[Tile]
      // Pick the direction with the highest food smell (if any)
      nextTile = neighboursSortedByMap(neighbours, diffusionMaps._1, occupiedTiles)

      if (nextTile.isEmpty){
        // Next try and explore
        nextTile = neighboursSortedByMap(neighbours, diffusionMaps._2, occupiedTiles)
      }

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
