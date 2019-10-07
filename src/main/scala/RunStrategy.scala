import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.duration.Duration
import java.util.concurrent.CopyOnWriteArrayList
import scala.collection.JavaConverters._

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

class LinearCombonationEvaluator(private val _weights: List[(Double, Evaluator)]) extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    _weights.map({case (weight, evaluator) => weight * evaluator(board)}).sum
  }
}

class HeatCounter extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    val height = board.height
    -board.pieces.map((piece) => Math.pow(height - piece.row - 1, 2)).sum
  }
}

class EmptySquaresEnclosedCounter extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    -(board.numSquares - board.numPiecesOnBoard - board.columns.map(_.numEmptySquaresFromTop).sum)
  }
}

class EmptySquaresBoxedInColCounter extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    -board.nonEmptyRows.map(_.squares.count((square) => square.isEmpty && !square.toRight.isEmpty && !square.toLeft.isEmpty)).sum[Int]
  }
}

class EmptySquaresBoxedInCounter extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    -board.nonEmptyRows.map(_.squares.count((square) => square.isEmpty && !square.toRight.isEmpty && !square.toLeft.isEmpty && !square.above.isEmpty)).sum[Int]
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
  case class SimulationResult(harmonicMean: Double, mean: Double)

  // Run simulations concurrently
  def runMany(n: Int, strategyGenerator: () => Strategy): SimulationResult = {
    var scores = new CopyOnWriteArrayList[Double]()  // Thread safe
    def computation = {
      for(_ <- 0 to n) yield {
        val f = Future {
          val runStrategy = new RunStrategy(strategyGenerator())
          val score = runStrategy.run
          score
        }
        f.onComplete {
          case Success(score) => {
            scores.add(score.toDouble)
            println(score)
          }
          case Failure(e) => e.printStackTrace
        }
        f
      }
    }
    Await.result(Future.sequence(computation), Duration.Inf)  // Block
    val scoresList = scores.asScala.toList
    SimulationResult(scoresList.size / (scoresList.map(1. / _).sum), scoresList.sum / scoresList.size)
  }

  def main(args: Array[String]): Unit = {
    val evaluator = new LinearCombonationEvaluator(List(
      (1.3, new HeatCounter),
      (50.0, new EmptySquaresBoxedInColCounter),
      // (50.0, new EmptySquaresBoxedInCounter),
      (100.0, new EmptySquaresEnclosedCounter)))
    //val svs = new Simulation(strategy, sleepTime=0)
    //svs.run
    println(runMany(100, () => new SearchEvalStrategySingleMoves(evaluator)))
  }
}
