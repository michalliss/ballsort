package domain

import zio.{Random, ZIO}

case class Game(height: Int, probeCount: Int, probes: Vector[Probe]) {

  def hash = Set.from(probes.map(_.hash))

  def isSolved: Boolean = probes.forall(_.isSolved)

  def canMoveSingleBall(from: Int, to: Int) = {
    if from == to then false
    else if probes(from).isSolved then false
    else
      probes(from).peek match
        case None       => false
        case Some(ball) => probes(to).canAccept(ball)
  }

  def possibleMoves = {
    (for {
      from <- probes.indices
      to   <- probes.indices
      if canMoveSingleBall(from, to)
    } yield (from, to))
  }

  def move(from: Int, to: Int): Option[Game] = {
    for {
      ball    <- probes(from).peek
      newFrom <- probes(from).pop
      newTo   <- probes(to).append(ball)
    } yield Game(height, probeCount, probes.updated(from, newFrom).updated(to, newTo))
  }
}

object Game {

  def random(colorsCount: Int, height: Int) = {
    for {
      colors      <- ZIO.succeed(0 until colorsCount)
      allBalls    <- Random.shuffle(colors.flatMap(x => List.fill(height)(Ball.unsafeMake(x))).toList)
      filledProbes = allBalls.grouped(height).map(x => Probe(height, Vector.from(x))).toList
      emptyProbes  = (0 until 2).map(_ => Probe(height, Vector.empty)).toList
    } yield Game(height, colorsCount, (filledProbes ++ emptyProbes).toVector)
  }

  def sampleGame = Game(
    3,
    5,
    Vector(
      Probe(3, Vector(Ball((3)), Ball((1)), Ball((2)))),
      Probe(3, Vector(Ball((2)), Ball((1)), Ball((2)))),
      Probe(3, Vector(Ball((1)), Ball((3)), Ball((3)))),
      Probe(3, Vector.empty),
      Probe(3, Vector.empty)
    )
  )
}
