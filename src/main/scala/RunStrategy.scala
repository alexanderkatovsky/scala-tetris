import scala.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}
import scala.concurrent.duration.Duration

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

class ConditionalEvaluator(private val _fn: TetrisBoard => Evaluator) extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    _fn(board)(board)
  }
}

class HeatCounter extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    val height = board.height
    -board.pieces.map((piece) => Math.pow(height - piece.row - 1, 2)).sum
  }
}

class NumberOfPieces extends Evaluator {
  def apply(board: TetrisBoard): Double = {
    -board.numPiecesOnBoard
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
    -board.nonEmptyRows.map(_.squares.count((square) => square.isEmpty && !(square.toRight.isEmpty || square.toLeft.isEmpty || square.above.isEmpty || square.below.isEmpty))).sum[Int]
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
    var scores = List[Double]()
    def computation = {
      for(_ <- 0 to n) yield {
        val f = Future {
          val runStrategy = new RunStrategy(strategyGenerator())
          val score = runStrategy.run
          score
        }
        f.onComplete {
          case Success(score) => synchronized {
            scores :+= score.toDouble
            // println(score)
          }
          case Failure(e) => e.printStackTrace
        }
        f
      }
    }
    Await.result(Future.sequence(computation), Duration.Inf)  // Block
    SimulationResult(scores.size / (scores.map(1. / _).sum), scores.sum / scores.size)
  }

  def runStrategyWithConfiguration(a: Array[Double], nsim: Int=100): Double = {
    val evaluator = new LinearCombonationEvaluator(List(
      (a(0), new HeatCounter),
      (a(1), new EmptySquaresBoxedInColCounter),
      (a(2), new EmptySquaresBoxedInCounter),
      (a(3), new EmptySquaresEnclosedCounter)))
    runMany(nsim, () => new SearchEvalStrategy(evaluator)).harmonicMean
  }

  def runStrategyWithConfiguration2(a: Array[Double], nsim: Int=100): Double = {
    val evaluator = new LinearCombonationEvaluator(List(
      (a(0), new HeatCounter),
      (a(1), new EmptySquaresBoxedInColCounter),
      (a(2), new EmptySquaresBoxedInCounter),
      (a(3), new EmptySquaresEnclosedCounter),
      (a(4), new NumberOfPieces)))
    runMany(nsim, () => new SearchEvalStrategy(evaluator)).harmonicMean
  }

  def main(args: Array[String]): Unit = {
    // val a: Array[Double] = Array(0.5249136820544308, 24.429006558196026, 1.3261097078240618, 71.8004953210443)
    // val evaluator = new LinearCombonationEvaluator(List(
    //   (a(0), new HeatCounter),
    //   (a(1), new EmptySquaresBoxedInColCounter),
    //   (a(2), new EmptySquaresBoxedInCounter),
    //   (a(3), new EmptySquaresEnclosedCounter)))
    val a: Array[Double] = Array(0.699622016895185, 37.884364583911335, 3.7361015320832975, 98.53178383544285, 0.9647204495914462)
    val evaluator = new LinearCombonationEvaluator(List(
      (a(0), new HeatCounter),
      (a(1), new EmptySquaresBoxedInColCounter),
      (a(2), new EmptySquaresBoxedInCounter),
      (a(3), new EmptySquaresEnclosedCounter),
      (a(4), new NumberOfPieces)))
    // println(runStrategyWithConfiguration(a, 10))
    new Simulation(strategy=new SearchEvalStrategySingleMoves(evaluator), sleepTime=10).run
  }
}
