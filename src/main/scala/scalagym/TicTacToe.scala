package scalagym

import io.Source
import util.control.Breaks._

/**
* Scala TicTacToe game without any side effects
*/
object TicTacToe {
  val WinCount = 3

  sealed trait Move
  case object O extends Move
  case object X extends Move

  sealed abstract class Game[T](val board: T)
  case class InProgress[T](override val board: T) extends Game[T](board)
  case class Finished[T](override val board: T) extends Game[T](board)

  case class Position(x: Int, y: Int)
  type Board = Seq[Seq[Option[Move]]]

  def createGame(width: Int, height: Int): InProgress[Board] =
    InProgress(for (x <- 0 until width) yield for(y <- 0 until height) yield None)

  def move(game: InProgress[Board], p: Position): Game[Board] =
    game.board(p.x)(p.y) match {
      case Some(move) => throw new IllegalArgumentException("Position is already taken by " + move)
      case None => placeMove(game.board, p, whoseTurn(game)) match {
        case board if finished_?(board) => Finished(board)
        case board => InProgress(board)
      }
    }

  def whoseTurn(game: InProgress[Board]): Move =
    game.board.flatMap(_.flatten).foldLeft((0, 0)) {
      case ((x, o), X) => (x + 1, o)
      case ((x, o), O) => (x, o + 1)
    } match {
      case (x, o) if x - o <= 0 => X
      case _ => O
    }

  def whoWon(game: Finished[Board]): Option[Move] =
    (for {
      x <- 0 until game.board.size
      y <- 0 until game.board(0).size
      curr <- game.board(x)(y)
      if won_?(game.board, curr, x, y)
    } yield Some(curr)) find (_.isDefined) getOrElse None

  def playerAt(game: Game[Board], p: Position): Option[Move] = game.board(p.x)(p.y)

  def draw(game: Game[Board]) = (for (y <- 0 until game.board(0).size) yield horizMoves(game.board, 0, y) map {
    case Some(m) => m.toString
    case None => " "
  } mkString " | ") mkString ("\n" + ("-" * game.board.size).mkString("-+-") + "\n")

  private def won_?(board: Board, m: Move, x: Int, y: Int): Boolean =
    won_?(horizMoves(board, x, y), m) || won_?(vertMoves(board, x, y), m) ||
        won_?(diagRightMoves(board, x, y), m) || won_?(diagRightMoves(board, x, y), m)

  private def won_?(moves: Seq[Option[Move]], m: Move): Boolean = moves.foldLeft(List(0)) {
    case (count :: rest, Some(`m`)) => count + 1 :: rest
    case (counts, _) => 0 :: counts
  }.max >= WinCount

  private def horizMoves(board: Board, x: Int, y: Int) = for (xx <- x until board.size) yield board(xx)(y)
  private def vertMoves(board: Board, x: Int, y: Int) = for (yy <- y until board(x).size) yield board(x)(yy)
  private def diagRightMoves(board: Board, x: Int, y: Int) =
    for ((xx, yy) <- (x until board.size) zip (y until board(x).size)) yield board(xx)(yy)
  private def diagLeftMoves(board: Board, x: Int, y: Int) =
    for ((xx, yy) <- (0 to x by -1) zip (y until board(x).size)) yield board(xx)(yy)

  private def placeMove(board: Board, p: Position, m: Move) =
    board.updated(p.x, board(p.x).updated(p.y, Some(m)))

  private def finished_?(board: Board) =
    board.flatMap(_.flatten).size == board.size * board(0).size || whoWon(Finished(board)).isDefined
}

object Int {
  def unapply(s: String): Option[Int] = try {
    Some(s.toInt)
  } catch {
    case _ : java.lang.NumberFormatException => None
  }
}

object TicTacToeGame extends Application {
  import TicTacToe._

  val game = createGame(3, 3)

  println("Welcome to Tic Tac Toe!")
  println(draw(game))
  print(whoseTurn(game) + "> ")

  breakable {
    Source.stdin.getLines.map(_.split("\\s*,\\s*").toList match {
      case List(Int(x), Int(y)) if x < 3 && y < 3 => Some(Position(x, y))
      case _ => println("Invalid position, should be: x, y"); None
    }).filter(_.isDefined).map(_.get).foldLeft(game: Game[Board]) {
      case (g @ InProgress(_), p) =>
        move(g, p) match {
          case game @ InProgress(_) =>
            print(draw(game) + "\n" + whoseTurn(game) + "> "); game
          case game @ Finished(_) =>
            println(draw(game) + "\n" + "Game finished, " + whoWon(game) + " won!"); break; game
        }
      case (game, p) => println("Game is already finished!"); game
    }
  }

  println("Bye!")
}