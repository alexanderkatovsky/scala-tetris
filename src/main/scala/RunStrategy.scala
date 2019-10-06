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
    val actions = _board.getPlayerSingleActions
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
  def isFinished = _isFinished
  def score = _score
}


abstract class Strategy {
  def choosePlayerAction(game: TetrisGame): Option[PlayerAction]
}


class RunStrategy(val strategy: Strategy) {
  private var _game: TetrisGame = null
  def onPlayerMove = {}
  def onGameFinish = {}

  def run: Integer = {
    _game = new TetrisGame
    while(!_game.isFinished) {
      strategy.choosePlayerAction(_game) match {
        case Some(move) => {
          _game.executePlayerAction(move)
          onPlayerMove
        }
        case None => {}
      }
    }
    onGameFinish
    _game.score
  }

  def runMany(n: Int): Double = {
    var scores = List[Double]()
    for(_ <- 0 to n) {
      val score = run
      println(score)
      scores :+= score.toDouble
    }
    scores.sum / n
  }

  def game = _game
}

class Simulation(override val strategy: Strategy, val sleepTime: Long = 0) extends RunStrategy(strategy) {
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
  def apply(board: TetrisBoard): Double
}

class HeatCounter extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    val height = board.height
    -board.pieces.map((piece) => Math.pow(height - piece.row - 1, 2)).sum
  }
}

class SearchEvalStrategy(val evaluator: Evaluator) extends Strategy {
  def choosePlayerAction(game: TetrisGame): Option[TetrisBoard#PlayerDroppedAction] = {
    val actions = game.getPlayerDroppedActions
    if(actions.length > 0) {
      val action = actions.reduceLeft((a1, a2) => if(evaluator(a1.newBoard) > evaluator(a2.newBoard)) a1 else a2)
      Some(action)
    } else {
      None
    }
  }
}

class SearchEvalStrategySingleMoves(val evaluator: Evaluator) extends Strategy {
  private var _actions = List[TetrisBoard#PlayerSingleAction]()
  val strategy = new SearchEvalStrategy(evaluator)

  def choosePlayerAction(game: TetrisGame): Option[PlayerAction] = {
    if(_actions.isEmpty) {
      strategy.choosePlayerAction(game) match {
        case Some(mAction) => {
          _actions = mAction.singleActions
        }
        case None => {}
      }
    }
    if(!_actions.isEmpty) {
      val returnAction = Some(_actions.head)
      _actions = _actions.tail
      returnAction
    } else None
  }
}


object RunStrategy {
  def main(args: Array[String]): Unit = {
    // val strategy = new RandomStrategy
    val evaluator = new HeatCounter
    val strategy = new SearchEvalStrategySingleMoves(evaluator)
    val svs = new Simulation(strategy, sleepTime=10)
    svs.run
    // val svs = new RunStrategy(strategy)
    // println(svs.runMany(100))
  }
}
