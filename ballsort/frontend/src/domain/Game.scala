package ballsort.frontend.domain

import zio.{Random, ZIO}

case class Game(height: Int, probeCount: Int, probes: Vector[Probe]) {
  def hash              = Set.from(probes.map(_.hash))
  def colorsCount       = probeCount - 2
  def isSolved: Boolean = probes.forall(_.isSolved)

  def canMove(move: Move) = {
    if move.from == move.to then false
    else if probes(move.from).isSolved then false
    else if probes(move.to).isEmpty && probes(move.from).allSameColor then false
    else
      probes(move.from).peek match
        case None       => false
        case Some(ball) => probes(move.to).canAccept(ball)
  }

  def possibleMoves = {
    (for {
      from <- probes.indices
      to   <- probes.indices
      if canMove(Move(from, to))
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
      filledProbes = allBalls.grouped(height).map(x => Probe(height, List.from(x))).toList
      emptyProbes  = (0 until 2).map(_ => Probe(height, Nil)).toList
    } yield Game(height, colorsCount + 2, (filledProbes ++ emptyProbes).toVector)
  }

  def solved(colorsCount: Int, height: Int) = {
    for {
      colors      <- ZIO.succeed(0 until colorsCount)
      allBalls     = colors.flatMap(x => List.fill(height)(Ball.unsafeMake(x))).toList
      filledProbes = allBalls.grouped(height).map(x => Probe(height, List.from(x))).toList
      emptyProbes  = (0 until 2).map(_ => Probe(height, Nil)).toList
    } yield Game(height, colorsCount + 2, (filledProbes ++ emptyProbes).toVector)
  }

  def sampleGame = Game(
    3,
    5,
    Vector(
      Probe(3, List(Ball((2)), Ball((0)), Ball((1)))),
      Probe(3, List(Ball((1)), Ball((0)), Ball((1)))),
      Probe(3, List(Ball((0)), Ball((2)), Ball((2)))),
      Probe(3, List.empty),
      Probe(3, List.empty)
    )
  )
}
