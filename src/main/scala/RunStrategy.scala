import scala.util.Random


class TetrisGame {
  private var _board = new TetrisBoard
  private var _isFinished = false
  _addNewPiece

  def _addNewPiece = {
    val pieces = List(T, J, L, Z, S, I, O)
    val index = Random.nextInt(pieces.length)
    val piece = pieces(index)
    _board.addNewPiece(piece) match {
      case Some(new_board) => _board = new_board
      case None => _isFinished = true
    }
  }

  def getPlayerActions: List[PlayerAction] = {
    val actions = _board.getPlayerActions
    if(actions.length == 0) _isFinished = true
    actions
  }

  def executePlayerAction(action: PlayerAction): Int = {
    _board = _board.executePlayerAction(action)
    if(_board.pieceDropped) _addNewPiece
    1
  }

  def printBoard = _board.printBoard
  def board = _board
  def isFinished = _isFinished
}


abstract class Strategy {
  def choosePlayerAction(game: TetrisGame): Option[PlayerAction]
}

class RandomStrategy extends Strategy {
  def choosePlayerAction(game: TetrisGame): Option[PlayerAction] = {
    val actions = game.getPlayerActions
    if(actions.length > 0) {
      val index = Random.nextInt(actions.length)
      Some(actions(index))
    } else {
      None
    }
  }
}


object RunStrategy {
  val _sleepTime: Long = 10

  def run(strategy: Strategy, game: TetrisGame = new TetrisGame, isSimulation: Boolean = false): Integer = {
    var score: Integer = 0
    while(!game.isFinished) {
      strategy.choosePlayerAction(game) match {
        case Some(move) => {
          score += game.executePlayerAction(move)
          if(isSimulation) {
            game.printBoard
            Thread.sleep(_sleepTime)
          }
        }
        case None => {}
      }
    }
    if(isSimulation) println(f"score: $score")
    score
  }

  def main(args: Array[String]): Unit = {
    val svs = new RandomStrategy
    run(svs, isSimulation=true)
  }
}
