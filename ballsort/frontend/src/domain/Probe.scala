package ballsort.frontend.domain

import Ball.*

case class Probe(capacity: Int, balls: List[Ball]) {

  def hash = balls.map(_.color).mkString

  def freeSpace    = capacity - balls.size
  val isSolved     = balls.isEmpty || (balls.size == capacity && balls.distinct.size == 1)
  def allSameColor = balls.map(_.color).distinct.size == 1
  def isEmpty      = balls.isEmpty
  def isFull       = balls.size == capacity
  val peek         = balls.headOption match
    case None      => None
    case Some(top) => Some(balls.takeWhile(_.color == top.color))
  val peekList     = peek.toList.flatten

  def canAccept(ballsToAppend: List[Ball]) = {
    balls.headOption match
      case Some(h) => h.color == ballsToAppend.head.color && freeSpace >= ballsToAppend.size
      case None    => true
  }

  def pop: Option[Probe] = balls.headOption match
    case None      => None
    case Some(top) => Some(Probe(capacity, balls.dropWhile(_.color == top.color)))

  def append(ballsToAppend: List[Ball]): Option[Probe] =
    if canAccept(ballsToAppend) then Some(Probe(capacity, ballsToAppend ::: balls)) else None

}
