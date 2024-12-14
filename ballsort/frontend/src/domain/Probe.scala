package domain

import neotype.*

import Ball.*

case class Probe(capacity: Int, balls: Vector[Ball]) {

  def hash = balls.map(_.color).mkString

  val isSolved: Boolean = {
    balls.isEmpty || (balls.size == capacity && balls.forall(_.color == balls.head.color))
  }

  val peek = balls.headOption

  def canAccept(ball: Ball) = {
    balls.headOption match
      case Some(h) => h.color == ball.color && balls.size < capacity
      case None    => true
  }

  def pop: Option[Probe] = balls.headOption match
    case Some(_) => Some(Probe(capacity, balls.tail))
    case None    => None
  

  def append(ball: Ball): Option[Probe] = if canAccept(ball) then Some(Probe(capacity, ball +: balls)) else None

}
