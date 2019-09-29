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

sealed class Piece(val positions: Iterable[Point]) {
  def rotateLeft = new Piece(positions.map((p) => Point(-p.row, p.col)))
  def rotateRight = new Piece(positions.map((p) => Point(p.row, -p.col)))
}

case object T extends Piece(List((0, 0), (-1, 0), (1, 0), (0, -1)))
case object J extends Piece(List((0, 0), (-1, 0), (0, -1), (0, -2)))
case object L extends Piece(List((0, 0), (1, 0), (0, -1), (0, -2)))
case object Z extends Piece(List((0, 0), (-1, 0), (0, -1), (1, -1)))
case object S extends Piece(List((0, 0), (-1, -1), (0, -1), (1, 0)))
case object I extends Piece(List((0, 0), (0, -1), (0, -2), (0, -3)))
case object O extends Piece(List((0, 0), (0, -1), (-1, 0), (-1, -1)))

case class BoardPiece(piece: Piece, anchor: Point) {
  def positions = piece.positions.map(_ + anchor)
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

case class PlayerAction(piece: BoardPiece, board: TetrisBoard)

class TetrisBoard(val width: Int = 10, val height: Int = 20, private val _board: BitSet = BitSet(), private val _piece: Option[BoardPiece] = None) {
  def ==(other: TetrisBoard) = {
    (other._board == _board) && (other._piece == _piece)
  }
  
  def getPlayerActions: List[PlayerAction] = {
    _piece match {
      case Some(piece) => {
        var legal_actions = List[PlayerAction]()
        for (move <- Seq(Down, Left, Right, RotateLeft, RotateRight)) {
          val moved_piece = move.move(piece)
          if(_isLegal(moved_piece)) {
            legal_actions :+= PlayerAction(moved_piece, this)
          }
        }
        legal_actions
      }
      case None => List()
    }
  }

  def isTopRowOccupied = {
    (for(col <- 0 to width - 1) yield _boardValue((col, 0))).exists(_)
  }

  private def _startAnchor: Point = (width / 2, 0)

  def addNewPiece(piece: Piece) = {
    val bp = BoardPiece(piece, _startAnchor)
    if(_isLegal(bp)) Some(new TetrisBoard(width, height, _board, Some(bp)))
    else None
  }

  def pieceDropped = _piece == None

  def executePlayerAction(action: PlayerAction): TetrisBoard = {
    if(action.board != this)
      throw new Exception("illegal action")
    val piece = action.piece
    if(_hasPieceDropped(piece)) _addPieceToBoard(piece)
    else new TetrisBoard(width, height, _board, Some(piece))
  }

  private def _isLegal(piece: BoardPiece): Boolean = {
    !piece.positions.exists((x) => _boardValue(x) || x.row >= height || x.col < 0 || x.col >= width)
  }

  private def _pointToIndex(point: Point): Int = {
    point.row * width + point.col
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
}
