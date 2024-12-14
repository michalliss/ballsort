package ballsort.frontend

import com.raquo.laminar.api.L.*
import domain.Game
import domain.solver.BallSortSolver
import foxxy.frontend.utils.*
import zio.*
import be.doeraene.webcomponents.ui5.Input

case class HomePage() {

  def renderBall(ball: domain.Ball) = {
    div(
      textAlign.center,
      ball.color.toString,
      border       := "1px solid black",
      borderRadius := "50%",
      width        := "50px",
      height       := "50px"
    )
  }

  def renderGame(game: Game) = {
    div(
      game.probes.zipWithIndex.map { case (probe, index) =>
        div(
          probe.balls.map(renderBall),
          border       := "1px solid black",
          borderRadius := "5px",
          padding      := "5px",
          margin       := "5px",
          display      := "inline-block",
          onClick.mapToUnit --> { x =>
            println(s"Clicked on probe $index")
          }
        )
      }
    )
  }

  def render = ZIO.attempt {
    val colors = Var(0)
    val height = Var(0)
    val game   = Var[Option[Game]](Some(Game.sampleGame))

    div(
      child <-- game.signal.map(_.map(renderGame).getOrElse(div("No game"))),
      Input(
        value <-- colors.signal.map(_.toString),
        onInput.mapToValue.map(_.toInt) --> colors.writer
      ),
      Input(
        value <-- height.signal.map(_.toString),
        onInput.mapToValue.map(_.toInt) --> height.writer
      ),
      button(
        "Create random game",
        onClick.mapToUnit --> { x =>
          (for {
            gameValue <- Game.random(colors.now(), height.now())
            _          = game.update(_ => Some(gameValue))
          } yield ()).toFutureUnsafe
        }
      ),
      button(
        "Solve game",
        onClick.mapToUnit --> { x =>
          (for {
            gameValue <- ZIO.succeed(game.now().get)
            solution   = BallSortSolver.solve(gameValue)
            _          = println(solution)
          } yield ()).toFutureUnsafe
        }
      )
    )
  }
}
