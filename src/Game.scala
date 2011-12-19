import collection.mutable.{ListBuffer, HashMap, HashSet}
import scala.math.{abs,min,max,pow,sqrt}

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

  def getOffsets (squaredRadius: Int): HashSet[(Int, Int)] = {
    val radius = (sqrt(squaredRadius)+1).toInt
    // Top left corner of approximate view
    val middle = Tile(row = parameters.rows/2, column = parameters.columns/2)
    val topLeft = Tile(row = middle.row-radius, column = middle.column-radius)
    val offsets = HashSet.empty[(Int,  Int)]
    for (x <- 0 to radius*2){
      for (y <- 0 to radius*2){
        val thisTile = Tile(row = topLeft.row+x, column = topLeft.column+y)
        if (distanceFrom(thisTile).to(middle) <= squaredRadius){
          offsets += ((thisTile.row-middle.row, thisTile.column-middle.column))
        }
      }
    }

    offsets
  }

  def setupVisibility() {
    val offsets = getOffsets(parameters.viewRadius)

    // Use the offsets to figure out which tiles are visible
    for (antTile <- board.myAnts.keys){
      for (offset <- offsets){
        val coord = normalise(antTile.row, antTile.column, offset._1, offset._2)
        visibility(coord._1)(coord._2) = turn
      }
    }

  }

  def normalise(row: Int, col: Int, mRow: Int, mCol: Int) = {
    var newRow = row+mRow
    var newCol = col+mCol

    if (newRow > parameters.rows-1){
      newRow -= parameters.rows
    }
    else if (newRow < 0){
      newRow += parameters.rows
    }
    
    if (newCol > parameters.columns-1){
      newCol -= parameters.columns
    }
    else if (newCol < 0){
      newCol += parameters.columns
    }

    (newRow, newCol)
  }
  
  def distanceFrom(one: Tile) = new {
    def to(another: Tile) = {
      val dRow = abs(one.row - another.row)
      val dCol = abs(one.column - another.column)
      pow(min(dRow, parameters.rows - dRow), 2) + pow(min(dCol, parameters.columns - dCol), 2)
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

}

