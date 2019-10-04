import scala.util.Random


class TetrisGame {
  private var _board = new TetrisBoard
  private var _isFinished = false
  private var _score: Integer = 0
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
    _score += 1
    _score
  }

  def printBoard = _board.printBoard
  def board = _board
  def isFinished = _isFinished
  def score = _score
}


abstract class Strategy {
  def choosePlayerAction(game: TetrisGame): Option[PlayerAction]
}


class RunStrategy(val strategy: Strategy, val game: TetrisGame = new TetrisGame) {
  def onPlayerMove = {}
  def onGameFinish = {}

  def run: Integer = {
    while(!game.isFinished) {
      strategy.choosePlayerAction(game) match {
        case Some(move) => {
          game.executePlayerAction(move)
          onPlayerMove
        }
        case None => {}
      }
    }
    onGameFinish
    game.score
  }
}

class Simulation(override val strategy: Strategy, override val game: TetrisGame = new TetrisGame, val sleepTime: Long = 0) extends RunStrategy(strategy, game) {
  override def onPlayerMove = {
    game.printBoard
    Thread.sleep(sleepTime)
  }

  override def onGameFinish = println(f"score: ${game.score}")
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
  def main(args: Array[String]): Unit = {
    val svs = new Simulation(new RandomStrategy, sleepTime=50)
    svs.run
  }
}
