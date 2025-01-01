package ballsort.frontend.services.solver

import zio.*

object BacktrackingZIO {
  trait Problem {
    type Node

    trait Result
    case object Rejected                       extends Result
    case object Accepted                       extends Result
    case class Undecided(children: List[Node]) extends Result

    def root: Node
    def analyse(node: Node): ZIO[Any, Nothing, Result]

    final def solve(candidates: List[Node] = List(root)): ZIO[Any, Nothing, Option[Node]] = candidates match {
      case candidate :: tail =>
        for {
          analysisResult <- analyse(candidate)
          result         <- analysisResult match {
                              case Accepted            => ZIO.succeed(Some(candidate))
                              case Rejected            => solve(tail)
                              case Undecided(children) => solve(children ::: tail)
                            }
        } yield result
      case Nil               => ZIO.succeed(None)
    }
  }
}
