// Board and pieces design adapted from https://github.com/jaybutera/tetrisRL

import scala.collection.immutable.BitSet
import scala.collection.decorators._
import scala.language.implicitConversions

case class Point(col: Int, row: Int) {
  def oneBelow = Point(col, row + 1)
  def oneRight = Point(col + 1, row)
  def oneLeft = Point(col - 1, row)
  def +(other: Point) = Point(col + other.col, row + other.row)
}
object Point {
  implicit def pairToPoint(in: (Int, Int)) = Point(in._1, in._2)
}
import Point._

sealed abstract class Piece(val positions: List[Point]) {
  def rotateLeft = MovedPiece(positions.map((p) => Point(-p.row, p.col)))
  def rotateRight = MovedPiece(positions.map((p) => Point(p.row, -p.col)))
}

case class MovedPiece(override val positions: List[Point]) extends Piece(positions)

case object T extends Piece(List((0, 0), (-1, 0), (1, 0), (0, -1)))
case object J extends Piece(List((0, 0), (-1, 0), (0, -1), (0, -2)))
case object L extends Piece(List((0, 0), (1, 0), (0, -1), (0, -2)))
case object Z extends Piece(List((0, 0), (-1, 0), (0, -1), (1, -1)))
case object S extends Piece(List((0, 0), (-1, -1), (0, -1), (1, 0)))
case object I extends Piece(List((0, 0), (0, -1), (0, -2), (0, -3)))
case object O extends Piece(List((0, 0), (0, -1), (-1, 0), (-1, -1)))

case class BoardPiece(piece: Piece, anchor: Point) {
  def positions = piece.positions.map(_ + anchor)
  def maxRow = positions.map(_.row).max
}

sealed abstract class Move {
  def move(piece: BoardPiece) : BoardPiece
}
case object Down extends Move {
  def move(piece: BoardPiece) : BoardPiece = {
    new BoardPiece(piece.piece, piece.anchor.oneBelow)
  }
}
case object Left extends Move {
  def move(piece: BoardPiece) : BoardPiece = {
    new BoardPiece(piece.piece, piece.anchor.oneLeft.oneBelow)
  }
}
case object Right extends Move {
  def move(piece: BoardPiece) : BoardPiece = {
    new BoardPiece(piece.piece, piece.anchor.oneRight.oneBelow)
  }
}
case object RotateLeft extends Move {
  def move(piece: BoardPiece) : BoardPiece = {
    new BoardPiece(piece.piece.rotateLeft, piece.anchor.oneBelow)
  }
}
case object RotateRight extends Move {
  def move(piece: BoardPiece) : BoardPiece = {
    new BoardPiece(piece.piece.rotateRight, piece.anchor.oneBelow)
  }
}

trait PlayerAction {
  def execute: TetrisBoard
  def nPlayerMoves: Integer = 1
}

class TetrisBoard(val width: Int = 10, val height: Int = 20, private val _board: BitSet = BitSet(), private val _piece: Option[BoardPiece] = None) {
  case class PlayerSingleAction(piece: BoardPiece) extends PlayerAction {
    def execute = {
      if(_hasPieceDropped(piece)) _addPieceToBoard(piece)
      else new TetrisBoard(width, height, _board, Some(piece))
    }
  }
  case class PlayerDroppedAction(singleActions: List[PlayerSingleAction], newBoard: TetrisBoard) extends PlayerAction {
    def execute = newBoard
    override def nPlayerMoves = singleActions.size
  }

  def ==(other: TetrisBoard) = {
    (other._board == _board) && (other._piece == _piece)
  }
  
  def _getPlayerSingleActions(piece: BoardPiece, moves: Seq[Move] = Seq(Down, Left, Right, RotateLeft, RotateRight)): List[PlayerSingleAction] = {
    var legal_actions = List[PlayerSingleAction]()
    for (move <- moves) {
      val moved_piece = move.move(piece)
      if(_isLegal(moved_piece)) {
        legal_actions :+= PlayerSingleAction(moved_piece)
      }
    }
    legal_actions
  }

  def getPlayerSingleActions: List[PlayerSingleAction] = {
    _piece match {
      case Some(piece) => _getPlayerSingleActions(piece)
      case None => List()
    }
  }

  private def _dropToWithin(nRows: Int): List[PlayerSingleAction] = {
    var piece_ = _piece
    var actions = List[PlayerSingleAction]()
    while(true) {
      piece_ match {
        case Some(piece) => {
          if(minRow - piece.maxRow <= nRows) return actions
          actions ++= _getPlayerSingleActions(piece, Seq(Down))
          piece_ = Some(actions.last.piece)
        }
        case None => return actions
      }
    }
    actions
  }

  def getPlayerDroppedActions: List[PlayerDroppedAction] = {
    val dropActions = _dropToWithin(8)
    var piece = _piece
    if(!dropActions.isEmpty) piece = Some(dropActions.last.piece)
    piece match {
      case Some(piece) => {
        var finalBoards: Map[BitSet, List[PlayerSingleAction]] = Map()
        var alreadySeen: Set[Set[Point]] = Set()
        var tetrisBoards: Map[Set[Point], (BoardPiece, List[PlayerSingleAction])] = Map(piece.positions.toSet -> (piece, dropActions))
        while(!tetrisBoards.isEmpty) {
          var newTetrisBoards: Map[Set[Point], (BoardPiece, List[PlayerSingleAction])] = Map()
          for((piece, actions) <- tetrisBoards.values) {
            for(action <- _getPlayerSingleActions(piece)) {
              val newBoard = action.execute
              newBoard._piece match {
                case Some(piece) => {
                  val posSet = piece.positions.toSet
                  if(!alreadySeen.contains(posSet)) newTetrisBoards += (posSet -> (piece, actions :+ action))
                }
                case None => finalBoards += (newBoard._board -> (actions :+ action))
              }
            }
          }
          alreadySeen ++= newTetrisBoards.keys
          tetrisBoards = newTetrisBoards
        }
        finalBoards.toList.map({case (board, actions) => PlayerDroppedAction(actions, new TetrisBoard(width, height, board, None))})
      }
      case None => List()
    }
  }

  def isTopRowOccupied = {
    (for(col <- 0 to width - 1) yield _boardValue((col, 0))).exists(_)
  }

  def numPiecesOnBoard = {
    _board.size
  }

  private def _startAnchor: Point = (width / 2, 0)

  def addNewPiece(piece: Piece) = {
    val bp = BoardPiece(piece, _startAnchor)
    if(_isLegal(bp)) Some(new TetrisBoard(width, height, _board, Some(bp)))
    else None
  }

  def minRow: Int = {
    if(_board.isEmpty) height
    else _board.min / width
  }

  def pieces: Seq[Point] = {
    for(x <- _board.toSeq) yield _indexToPoint(x)
  }

  def pieceDropped = _piece == None

  private def _isLegal(piece: BoardPiece): Boolean = {
    !piece.positions.exists((x) => _boardValue(x) || x.row >= height || x.col < 0 || x.col >= width)
  }

  private def _pointToIndex(point: Point): Int = {
    point.row * width + point.col
  }

  private def _indexToPoint(index: Int): Point = {
    return Point(index % width, index / width)
  }

  private def _boardValue(point: Point): Boolean = {
    if(point.row >= 0 && point.col >= 0) {
      val index = _pointToIndex(point)
      _board(index)
    } else false
  }

  private def _hasPieceDropped(piece: BoardPiece): Boolean = {
    piece.positions.exists((x) => (x.row == height - 1) || _boardValue(x.oneBelow))
  }

  private def _eliminateWholeRows(board: BitSet): BitSet = {
    val i_start = width * (height - 1)
    var row_i = BitSet(i_start to i_start + width - 1:_*)
    var n_rows_eliminated = 0
    var new_board = BitSet()
    while(!(board & row_i).isEmpty) {
      if((board & row_i) == row_i) n_rows_eliminated += 1
      else new_board ++= (board & row_i) << (n_rows_eliminated * width)
      row_i >>= width
    }
    new_board
  }

  private def _addPieceToBoard(piece: BoardPiece): TetrisBoard = {
    val new_board = _board ++ piece.positions.map(_pointToIndex(_)).filter(_>=0)
    new TetrisBoard(width, height, _eliminateWholeRows(new_board))
  }

  private def _isOnBoard(point: Point) = {
    if(_boardValue(point)) true
    else {
      _piece match {
        case Some(piece) => piece.positions.exists(_==point)
        case None => false
      }
    }
  }

  def printBoard: Unit = {
    val line = " " + "-" * width
    println(line)
    for(row <- 0 to height - 1) {
      val st = "|" + (for (col <- (0 to width - 1)) yield (if(_isOnBoard((col, row))) "X" else " ")).mkString("") + "|"
      println(st)
    }
    println(line)
  }

  case class Square(val row: Int, val col: Int) {
    def isEmpty = {
      if(row >= 0 && col >= 0 && row < height && col < width) {
        val index = _pointToIndex((col, row))
        !_board(index)
      } else false
    }

    def toLeft = Square(row, col - 1)
    def toRight = Square(row, col + 1)
    def above = Square(row - 1, col)
    def below = Square(row + 1, col)
  }

  case class Column(val col: Int) {
    def numEmptySquaresFromTop: Int = {
      val numbersInCol = _board.filter(_ % width == col)
      if(numbersInCol.isEmpty) height
      else {
        val point = _indexToPoint(numbersInCol.min)
        point.row
      }
    }
  }

  case class Row(val row: Int) {
    def squares = for(col <- 0 to width - 1) yield Square(row, col)
  }

  def columns = {
    for(col <- 0 to width - 1) yield Column(col)
  }

  def nonEmptyRows = {
    for(row <- height - 1 to minRow by -1) yield Row(row)
  }

  def numSquares = height * width
}

object TetrisBoard {
  val pieces = List(T, J, L, Z, S, I, O)
}
