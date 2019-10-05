import scala.util.Random

class TetrisGame {
  private var _board = new TetrisBoard
  private var _isFinished = false
  private var _score: Integer = 0
  _addNewPiece

  def _addNewPiece = {
    val pieces = TetrisBoard.pieces
    val index = Random.nextInt(pieces.length)
    val piece = pieces(index)
    _board.addNewPiece(piece) match {
      case Some(new_board) => _board = new_board
      case None => _isFinished = true
    }
  }

  def getPlayerSingleActions: List[TetrisBoard#PlayerSingleAction] = {
    val actions = board.getPlayerSingleActions
    if(actions.length == 0) _isFinished = true
    actions
  }

  def getPlayerDroppedActions: List[TetrisBoard#PlayerDroppedAction] = {
    val actions = _board.getPlayerDroppedActions
    if(actions.length == 0) _isFinished = true
    actions
  }

  def executePlayerAction(action: PlayerAction): Int = {
    _board = action.execute
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
    val actions = game.getPlayerSingleActions
    if(actions.length > 0) {
      val index = Random.nextInt(actions.length)
      Some(actions(index))
    } else {
      None
    }
  }
}


abstract class Evaluator {
  def apply(action: TetrisBoard#PlayerDroppedAction): Double
}


class BasicCountEvaluator extends Evaluator {
  def apply(action: TetrisBoard#PlayerDroppedAction): Double = {
    action.newBoard.minRow - action.newBoard.numPiecesOnBoard
  }
}


class SearchEvalStrategy(val evaluator: Evaluator) extends Strategy {
  def choosePlayerAction(game: TetrisGame): Option[PlayerAction] = {
    val actions = game.getPlayerDroppedActions
    if(actions.length > 0) {
      val action = actions.reduceLeft((a1, a2) => if(evaluator(a1) > evaluator(a2)) a1 else a2)
      Some(action)
    } else {
      None
    }
  }
}


object RunStrategy {
  def main(args: Array[String]): Unit = {
    for(x <- 0 to 100) {
      // val strategy = new RandomStrategy
    val evaluator = new BasicCountEvaluator
    val strategy = new SearchEvalStrategy(evaluator)
    // val svs = new Simulation(strategy, sleepTime=100)
    val svs = new RunStrategy(strategy)
      println(svs.run)
    }
  }
}
