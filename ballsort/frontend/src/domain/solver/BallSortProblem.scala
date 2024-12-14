package domain.solver

import domain.Game

object BallSortSolver {

  case class Solution(moves: List[(Int, Int)])

  def solve(game: Game): Option[Solution] = {
    BallSortProblem.solve(List(BallSortProblem.BallSortNode(Set(), Nil, game))).map(x => Solution(x.moveHistory))
  }

  object BallSortProblem extends Backtracking.Problem {
    type Hash = Game

    case class BallSortNode(visited: Set[Hash], moveHistory: List[(Int, Int)], game: Game)
    type Node = BallSortNode
    def root = BallSortNode(Set(), Nil, Game.sampleGame)

    def analyse(node: BallSortNode) = {
      val isSolved = node.game.isSolved

      if (isSolved) Accepted
      else if (!isSolved && node.game.possibleMoves.isEmpty) Rejected
      else
        Undecided(
          node.game.possibleMoves
            .flatMap(x => {
              val newGame = node.game.move(x._1, x._2).get
              if (node.visited.contains(newGame)) then None
              else Some(BallSortNode(node.visited + newGame, x :: node.moveHistory, newGame))
            })
            .toList
        )
    }
  }

}
