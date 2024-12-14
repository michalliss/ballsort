package domain.solver

import scala.annotation.tailrec

// https://gist.github.com/tyrcho/a2275da90788b3c0cd79
object Backtracking {
  trait Problem {
    type Node

    trait Result
    case object Rejected                       extends Result
    case object Accepted                       extends Result
    case class Undecided(children: List[Node]) extends Result

    def root: Node
    def analyse(node: Node): Result

    @tailrec
    final def solve(candidates: List[Node] = List(root)): Option[Node] = candidates match {
      case candidate :: tail =>
        analyse(candidate) match {
          case Accepted            => Some(candidate)
          case Rejected            =>
            solve(tail)
          case Undecided(children) =>
            solve(children ::: tail)
        }
      case Nil               => None
    }
  }
}

