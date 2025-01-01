package ballsort.frontend
import be.doeraene.webcomponents.ui5.configkeys.InputType
import be.doeraene.webcomponents.ui5.{Bar, Button, Input, Label}
import com.raquo.laminar.api.L.*
import foxxy.frontend.utils.*
import zio.*

import domain.Game
import ballsort.frontend.services.SolverService

case class HomePage(solverService: SolverService) {

  val DEFAULT_COLORS = Map(
    0  -> "white",
    1  -> "red",
    2  -> "green",
    3  -> "blue",
    4  -> "yellow",
    5  -> "cyan",
    6  -> "magenta",
    7  -> "orange",
    8  -> "purple",
    9  -> "brown",
    10 -> "pink",
    11 -> "grey",
    12 -> "lightblue",
    13 -> "lightgreen",
    14 -> "lightyellow",
    15 -> "lightcyan",
    16 -> "lightmagenta",
    17 -> "lightorange",
    18 -> "lightpurple",
    19 -> "lightbrown",
    20 -> "lightpink"
  )

  val DEFAULT_HEIGHT       = 5
  val DEFAULT_COLORS_COUNT = 5

  def render = ZIO.attempt {
    val colors       = Var(DEFAULT_COLORS_COUNT)
    val heightSignal = Var(DEFAULT_HEIGHT)
    val colorTable   = Var(DEFAULT_COLORS)
    val game         = Var[Option[Game]](None)

    hDiv(
      onMountCallback { _ =>
        (for {
          gameValue <- Game.random(DEFAULT_COLORS_COUNT, DEFAULT_HEIGHT)
          _          = game.update(_ => Some(gameValue))
        } yield ()).toFutureUnsafe
      },
      vDiv(
        Bar(
          height := "6em",
          _.slots.endContent   := hDiv(
            alignItems.center,
            ColorConfigurationDialog().render(game.signal, colorTable)
          ),
          _.slots.startContent := hDiv(
            alignItems.center,
            gap := "1em",
            vDivA(
              Label("Colors"),
              Input(
                _.tpe := InputType.Number,
                value <-- colors.signal.map(_.toString),
                onInput.mapToValue.map(_.toInt) --> colors.writer
              )
            ),
            vDivA(
              Label("Height"),
              Input(
                _.tpe := InputType.Number,
                value <-- heightSignal.signal.map(_.toString),
                onInput.mapToValue.map(_.toInt) --> heightSignal.writer
              )
            ),
            vDivA(
              Label(""),
              Button(
                "Create random game",
                onClick.mapToUnit --> { x =>
                  (for {
                    gameValue <- Game.random(colors.now(), heightSignal.now())
                    _          = game.update(_ => Some(gameValue))
                  } yield ()).toFutureUnsafe
                }
              )
            )
          )
        ),
        child <-- game.signal.map(x =>
          x.map(g => GameView(solverService).renderInteractiveGame(g, colorTable.signal)).getOrElse(div("No game"))
        )
      )
    )
  }
}
