package ballsort.frontend.test

import zio.*
import zio.test.*
import zio.test.Assertion.*
import ballsort.frontend.domain.Game
import ballsort.frontend.services.SolverService

object DomainTest extends ZIOSpecDefault {
  def spec = suite("Domain test")(
    test("Game possible moves") {
      assertTrue(Game.sampleGame.possibleMoves.size == 6)
    },
    test("Moving a ball") {
      val game    = Game.sampleGame
      val newGame = game.move(0, 3).get
      assertTrue(newGame.probes(0).balls.size == 2)
      assertTrue(newGame.probes(3).balls.size == 1)
    },
    test("Illegal move is None") {
      val game    = Game.sampleGame
      val newGame = game.move(0, 1)
      assert(newGame)(isNone)
    },
    test("Random games can be generated") {
      for {
        game <- Game.random(3, 5)
      } yield assertTrue(game.probes.size == 5) && assertTrue(game.probes(0).balls.size == 5)
    },
    test("Random games can be solved") {
      for {
        _    <- Random.setSeed(2137)
        game <- Game.random(8, 6)
        res  <- ZIO.serviceWithZIO[SolverService](_.solveZIO(game))
      } yield assertTrue(res.isDefined)
    }
  ).provide(ZLayer.derive[SolverService])
}
