import collection.mutable.{ListBuffer, HashMap, HashSet}
import scala.math.{abs,min,max,pow}

case class GameInProgress(turn: Int = 0,
                          parameters: GameParameters = GameParameters(),
                          board: Board = Board(),
                          visibility: Array[Array[Int]] = Array.empty,
                          turnStartTime: Long = System.currentTimeMillis()) extends Game {
  val gameOver = false
  def including[P <: Positionable](positionable: P) = this.copy(board = this.board including positionable)
  def including(p: Positionable*): GameInProgress = p.foldLeft(this){(game, positionable) => game.including(positionable)}
}
case class GameOver(turn: Int = 0, 
                    parameters: GameParameters = GameParameters(), 
                    board: Board = Board(),
                    visibility: Array[Array[Int]] = Array.empty,
                    turnStartTime: Long = System.currentTimeMillis()) extends Game {
  val gameOver = true
}

sealed trait Game {
  val turn: Int
  val parameters: GameParameters
  val board: Board
  val gameOver: Boolean
  val visibility: Array[Array[Int]]
  val turnStartTime: Long

  def setupVisibility = {

    for (row <- 0 to parameters.rows-1){
      for (col <- 0 to parameters.columns-1){
        val thisTile = Tile(row = row, column = col)

        // If any of my ants are in the view radius then it is visible
        // Set visibilty of tiles viewable in this round to the round number
        if (board.myAnts.keys.exists{a =>
          distanceFrom(thisTile).to(a) <= parameters.viewRadius
        }){
          visibility(row)(col) = turn
        }
      }
    }
  }

  def distanceFrom(one: Tile) = new {
    def to(another: Tile) = {
      val dRow = abs(one.row - another.row)
      val dCol = abs(one.column - another.column)
      pow(min(dRow, parameters.rows - dRow), 2) + pow(min(dCol, parameters.columns - dCol), 2)
    }
  }

  def manhattaDistanceFrom(one: Tile) = new {
    def to(another: Tile): Int = {
      min(abs(one.row - another.row), min(one.row, another.row)+(parameters.rows-max(one.row, another.row)))
       + min(abs(one.column - another.column), min(one.column, another.column)+(parameters.columns-max(one.column, another.column)))
    }
  }

  def directionFrom(one: Tile) = new {
    def to(other: Tile): Set[CardinalPoint] = {
      val ns: Set[CardinalPoint] = if (one.row < other.row) {
        if (other.row - one.row >= parameters.rows / 2) Set(North) else Set(South)
      } else if (one.row > other.row) {
        if (one.row - other.row >= parameters.rows / 2) Set(South) else Set(North)
      } else Set()

      val ew: Set[CardinalPoint] = if (one.column < other.column) {
        if (other.column - one.column >= parameters.columns / 2) Set(West) else Set(East)
      } else if (one.column > other.column) {
        if (one.column - other.column >= parameters.columns / 2) Set(East) else Set(West)
      } else Set()

      ns ++ ew
    }
  }

  def tile(aim: CardinalPoint) = new {
    def of(tile: Tile) = {
      aim match {
        case North => tile.copy(row = if (tile.row == 0) parameters.rows - 1 else tile.row - 1)
        case South => tile.copy(row = (tile.row + 1) % parameters.rows)
        case East => tile.copy(column = (tile.column + 1) % parameters.columns)
        case West => tile.copy(column = if (tile.column == 0) parameters.columns - 1 else tile.column - 1)
      }
    }
  }

  def neighboursOf(tile: Tile): List[Tile] = {
    List(this.tile(North).of(tile), this.tile(East).of(tile), this.tile(South).of(tile), this.tile(West).of(tile))
  }

  def closestTo(target: Tile, limit: Int = 1) = {
    board.myAnts.values.toList.sortBy(a => distanceFrom(a.tile).to(target)).take(limit)
  }

  def closestFood(ant: MyAnt) = {
    board.food.keys.toList.sortBy(f => distanceFrom(ant.tile).to(f)).head
  }

  // Gets the route from one tile to another
  def route(from: Tile, target: Tile, occupiedTiles: HashSet[Tile] = HashSet.empty[Tile]) = {
      // Use the A* algorithm

      val openList = new ListBuffer[Tile]
      val closedList = new ListBuffer[Tile]
      val scores = new HashMap[Tile, Int]
      val parents = new HashMap[Tile, Tile]

      openList += from
      scores += from -> 0
      while (!openList.isEmpty){
        val currentTile = openList.sortBy(t => scores.apply(t)).head
        closedList += currentTile
        openList -= currentTile

        if (closedList.contains(target)){
          // Path found, clear openList so we terminate
          openList.clear()
        }
        else{
          val neighbours = neighboursOf(currentTile).filter(t => !board.water.contains(t) && !board.myHills.contains(t) && !occupiedTiles.contains(t) && !closedList.contains(t))
          neighbours.foreach(neighbour => {
              if (!openList.contains(neighbour)){
                // Add to openList and compute it's score A + B
                // A = parentScore + 1
                // B = estimatedDistance to targer
                openList += neighbour
                scores += neighbour -> (scores.apply(currentTile)+1+manhattaDistanceFrom(neighbour).to(target))
                parents += neighbour -> currentTile
              }
              else{
                // Tile is already in the open set. Update score if new score is lower
                val newScore = scores.apply(currentTile)+1+manhattaDistanceFrom(neighbour).to(target)
                if (newScore < scores.apply(neighbour)){
                  scores += neighbour -> newScore
                  parents += neighbour -> currentTile
                }
              }
          })
        }
      }

      // Should have the path now, walk backwards along the parent
      val bestRoute = new ListBuffer[Tile]
      var currentTile = target

      while (currentTile != from && parents.contains(currentTile)){
        bestRoute += currentTile
        currentTile = parents.apply(currentTile)
      }

      bestRoute.reverse.toList
    }
}

