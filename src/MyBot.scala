import collection.mutable.{HashSet}
import scala.math.{max, min}

object MyBot extends App {
  new AntsGame().run(new MyBot)
}

class MyBot extends Bot {
  var oldFoodMap = Array.empty[Array[Double]]
  var oldExploreMap = Array.empty[Array[Double]]

  var knownEnemyHills = HashSet.empty[Tile]

  var attackMap = Array.empty[Array[Double]]

  def diffuse(game: Game, maxIterations: Int = 200, bailoutMilliseconds: Long = 5) = {
    val maxValue = Float.MaxValue/4 // Need to divide by 4 since we are averaging lower down

    if (oldFoodMap.isEmpty){
      oldFoodMap = Array.ofDim[Double](game.parameters.rows, game.parameters.columns)
    }
    
    if (oldExploreMap.isEmpty){
      oldExploreMap = Array.ofDim[Double](game.parameters.rows, game.parameters.columns)
    }
    
    var newFoodMap = Array.ofDim[Double](game.parameters.rows, game.parameters.columns)
    var newExploreMap = Array.ofDim[Double](game.parameters.rows, game.parameters.columns)
    
    var bailout = false
    var averageTime: Long = 0
    var iterations: Long = 0
    val diffusionStartTime = System.currentTimeMillis()

    while (!bailout){
      newFoodMap = Array.ofDim[Double](game.parameters.rows, game.parameters.columns)
      newExploreMap = Array.ofDim[Double](game.parameters.rows, game.parameters.columns)

      (0 to game.parameters.rows-1).foreach{row =>
        (0 to game.parameters.columns-1).foreach{col =>
          val tile = Tile(row = row, column = col)
          var doFood = true
          var doExplore = true
          var foodDiffuseFactor = 1.0
          var exploreDiffuseFactor = 1.0

          if (game.board.water.contains(tile))
          {
            newFoodMap(row)(col) = 0
            newExploreMap(row)(col) = 0
          }
          else{
            if (game.board.myAnts.contains(tile)){
              foodDiffuseFactor = 0
              exploreDiffuseFactor = 0.5
            }
            // Food and hills
            else if (game.board.food.contains(tile)){
              newFoodMap(row)(col) = maxValue
              doFood = false
            }
            else if(knownEnemyHills.contains(tile)){
              newFoodMap(row)(col) = maxValue
              doFood = false
            }

            if (game.visibility(row)(col) != game.turn){
              newExploreMap(row)(col) = (maxValue/game.parameters.turns) * (game.parameters.turns - game.visibility(row)(col))
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

                val newValue = foodDiffuseFactor * (0.25 * (northFood + eastFood + southFood + westFood))
                newFoodMap(tile.row)(tile.column) = newValue
              }

              if (doExplore){
                // Diffuse explore
                val northExplore = oldExploreMap(northTile.row)(northTile.column)
                val eastExplore = oldExploreMap(eastTile.row)(eastTile.column)
                val southExplore = oldExploreMap(southTile.row)(southTile.column)
                val westExplore = oldExploreMap(westTile.row)(westTile.column)

                val newValue = exploreDiffuseFactor * (0.25 * (northExplore + eastExplore + southExplore + westExplore))
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

  def defend(game: Game, occupiedTiles: HashSet[Tile], antsPerDefender: Int = 5): Map[Tile,  Option[CardinalPoint]] = {
    val defenseOffsets = List((1,1), (1,-1), (-1,1), (-1,-1))
    val defensePositions = defenseOffsets.flatMap(o =>
      game.board.myHills.keys.map(h =>
        game.normalise(h.row, h.column, o._1, o._2))).map(t => Tile(row = t._1, column = t._2)).filter(t => !game.board.water.contains(t))

    val numberOfDefenders = min(defensePositions.size, game.board.myAnts.size / antsPerDefender)

    defensePositions.take(numberOfDefenders).flatMap{pos =>
      if (game.board.myAnts.contains(pos)){
        // Already have an ant there, don't move
        Map(pos -> Option.empty[CardinalPoint])
      }
      else{
        val closestAnt = game.closestTo(pos, defenseOffsets.size+1).filter(!defensePositions.contains(_)).head.tile
        val direction = game.directionFrom(closestAnt).to(pos).filter{d =>
          val tile = game.tile(d).of(closestAnt)
          !game.board.water.contains(tile) && !occupiedTiles.contains(tile)
        }.headOption
        Map(closestAnt -> direction)
      }
    }.toMap
  }

  def ordersFrom(game: Game): Set[Order] = {

    def neighboursSortedByMap(neighbours: List[Tile],  diffusionMap: Array[Array[Double]], occupiedTiles: HashSet[Tile]) = {
      val sortedNeighbours = neighbours.sortBy(n => diffusionMap(n.row)(n.column)).reverse
      sortedNeighbours.find(n => !game.board.water.contains(n) && !occupiedTiles.contains(n) && !game.board.myHills.contains(n) && (diffusionMap(n.row)(n.column) != 0) && attackMap(n.row)(n.column) >= 1)
    }

    def updateEnemyHills() {
      // Add all hills to known hills
      game.board.enemyHills.keys.foreach{ h =>
        knownEnemyHills += h
      }

      // Remove enemy hills we have razed
      knownEnemyHills.foreach{ h =>
        if (game.board.myAnts.keys.toList.contains(h)){
          knownEnemyHills -= h
        }
      }
    }

    def updateAttackMap() {
      attackMap = Array.ofDim(game.parameters.rows, game.parameters.columns)

      var offsets = game.getOffsets(game.parameters.attackRadius)
      val originalOffsets = offsets.clone()

      // Extend attack radius by one square to show all possible the ant COULD attack in the next round
      for (offset <- originalOffsets){
        offsets += ((offset._1+1, offset._2))
        offsets += ((offset._1-1, offset._2))
        offsets += ((offset._1, offset._2+1))
        offsets += ((offset._1, offset._2-1))
      }

      for (enemy <- game.board.enemyAnts.keys){
        for (offset <- offsets){
          val coord = game.normalise(enemy.row, enemy.column, offset._1, offset._2)
          attackMap(coord._1)(coord._2) -= 1
        }
      }

      for (myAnt <- game.board.myAnts.keys){
        for (offset <- offsets){
          val coord = game.normalise(myAnt.row, myAnt.column, offset._1, offset._2)
          attackMap(coord._1)(coord._2) += 1
        }
      }
    }

    val directions = List(North, East, South, West)
    val allAnts = game.board.myAnts.keys
    val occupiedTiles = new HashSet[Tile]
    allAnts.foreach(ant => occupiedTiles += ant)

    updateEnemyHills()
    val defenseMoves = defend(game, occupiedTiles)
    val antsStillToMove = allAnts.filterNot(defenseMoves.keySet.contains(_))
    val defenseOrders = defenseMoves.filterNot(_._2.isEmpty).map(d => Order(d._1, d._2.head))
    updateAttackMap()

    val visTime = System.currentTimeMillis()
    game.setupVisibility()
    System.err.println("Vis time: "+ (System.currentTimeMillis() - visTime).toString +"ms")

    val diffusionMaps = diffuse(game, 300, 30)

    System.err.println("Time left before moving ants: " + (game.parameters.turnTime - (System.currentTimeMillis() - game.turnStartTime)).toString)
    val antTime = System.currentTimeMillis()

    val result = antsStillToMove.flatMap{antTile =>
      val neighbours = game.neighboursOf(antTile)

      var nextTile = Option.empty[Tile]
      // Pick the direction with the highest food smell (if any)
      nextTile = neighboursSortedByMap(neighbours, diffusionMaps._1, occupiedTiles)

      if (nextTile.isEmpty){
        // Next try and explore
        nextTile = neighboursSortedByMap(neighbours, diffusionMaps._2, occupiedTiles)
      }

      val direction = directions.find{aim =>
        nextTile.nonEmpty && game.tile(aim).of(antTile) == nextTile.head
      }

      if (direction.nonEmpty){
        occupiedTiles -= antTile
        val newTile = game.tile(direction.head).of(antTile)
        occupiedTiles += newTile
      }

      // convert this (possible) direction into an order for this antTile
      direction.map{d => Order(antTile, d)}
    }.toSet ++ defenseOrders

    System.err.println("Ant movements: "+ (System.currentTimeMillis() - antTime).toString +"ms")

    result
  }
}
