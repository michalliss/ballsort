package ballsort.frontend

import animus.*
import com.raquo.laminar.api.L.*
import foxxy.frontend.utils.*
import zio.*

import domain.{Ball, Game}
import ballsort.frontend.services.SolverService
import ballsort.frontend.services.SolverService.Solution

case class GameView(solverService: SolverService) {
  private def renderProbe(
      capacity: Int,
      colors: Signal[Map[Int, String]],
      constBalls: Signal[List[Ball]],
      floatingBalls: Signal[List[Ball]],
      floatingStatus: Signal[Boolean],
      offeredBalls: Signal[List[Ball]],
      onClickHandler: Observer[Unit],
      onHoverHandler: Observer[Boolean]
  ) = vDivA(
    width        := "65px",
    maxHeight    := capacity * 65 + "px",
    height       := capacity * 65 + "px",
    flexDirection.columnReverse,
    border       := "1px solid grey",
    borderRadius := "5px",
    padding      := "5px",
    margin       := "5px",
    onClick.mapToUnit --> { x => onClickHandler.onNext(()) },
    onMouseEnter.mapToUnit --> { x => onHoverHandler.onNext(true) },
    onMouseLeave.mapToUnit --> { x => onHoverHandler.onNext(false) },
    children <-- constBalls.map(balls => balls.reverse.map(x => renderBall(x, colors))),
    children <-- floatingBalls.map { balls =>
      balls.map { ball =>
        div(
          renderBall(ball, colors),
          position.relative,
          top.px <-- floatingStatus.signal.map(if (_) -50.0 else 0.0).spring.map(_.toInt)
        )
      }
    },
    children <-- offeredBalls.signal.map(_.zipWithIndex).splitTransition(identity) {
      case (_, (ball, index), _, transition) => {
        div(
          renderBall(ball, colors),
          transition.blur(0.5),
          transition.offset(0.0, -100.0 - 20 * (index + 1)),
          transition.opacity
        )
      }
    }
  )

  private def renderBall(ball: domain.Ball, colors: Signal[Map[Int, String]]) = {
    div(
      textAlign.center,
      alignContent.center,
      ball.color.toString,
      border       := "2px solid grey",
      borderRadius := "50%",
      width        := "50px",
      height       := "50px",
      backgroundColor <-- colors.map(_.getOrElse(ball.color, "blue"))
    )
  }

  def renderBoard(initialGame: Game, gameUpdates: Signal[Game], onMove: Observer[(Int, Int)], colors: Signal[Map[Int, String]]) = {
    case class InteractionState(hovered: Option[Int], selected: Option[Int]) {
      def isHovered(index: Int)  = hovered.contains(index)
      def isSelected(index: Int) = selected.contains(index)
    }
    val interactionState = Var(InteractionState(None, None))

    enum Command:
      case Click(probeIndex: Int)
      case Hover(probeIndex: Int, hover: Boolean)

    val commandHandler = Observer[Command]:
      case Command.Click(probeIndex) =>
        interactionState.update: x =>
          x.selected match
            case None                               => x.copy(selected = Some(probeIndex))
            case Some(value) if value == probeIndex => x.copy(selected = None)
            case Some(value)                        => onMove.onNext((value, probeIndex)); InteractionState(None, None)

      case Command.Hover(probeIndex, hover) =>
        interactionState.update: x =>
          x.copy(hovered = if hover then Some(probeIndex) else None)

    hDivA(
      children <-- gameUpdates
        .map(_.probes)
        .splitByIndex((index, _, probeSignal) => {
          def ballsForProbe(probeIndex: Int) = Signal
            .combine(interactionState.signal, gameUpdates)
            .map { case (interaction, game) =>
              {
                for {
                  selected <- interaction.selected
                  hovered  <- interaction.hovered
                  if selected != probeIndex && hovered == probeIndex
                } yield game.probes(selected).peekList.take(game.probes(probeIndex).freeSpace)
              }.toList.flatten
            }
          vDivA(
            renderProbe(
              initialGame.height,
              colors,
              probeSignal.map(x => x.balls.drop(x.peek.map(_.size).getOrElse(0))),
              probeSignal.map(x => x.peek.toList.flatten),
              interactionState.signal.map(x => x.selected.contains(index)),
              probeSignal.flatMapSwitch(x => if x.isFull then Signal.fromValue(Nil) else ballsForProbe(index)),
              commandHandler.contramap(_ => Command.Click(index)),
              commandHandler.contramap(x => Command.Hover(index, x))
            ),
            div(s"$index", textAlign.center)
          )
        })
    )
  }

  def renderHint(solution: Option[Solution]) = {
    solution match {
      case Some(Solution(moves)) =>
        div(
          "Hint: ",
          moves.take(1).map { case (from, to) => s"$from -> $to" }.mkString(" | ")
        )
      case None                  => div("No solution found")
    }

  }

  def renderInteractiveGame(initialGame: Game, colors: Signal[Map[Int, String]]) = {
    val gameVar            = Var(initialGame)
    val onMoveObserver     = Observer[(Int, Int)] { case (from, to) => gameVar.update(x => { x.move(from, to).getOrElse(x) }) }
    def solveGame(g: Game) = solverService.solveZIO(g).disconnect.timeout(10.seconds).toEventStream
    val solution           = gameVar.signal.flatMapSwitch(x => solveGame(x))

    vDivA(
      alignItems.center,
      child <-- gameVar.signal.splitOne(x => (x.height, x.probeCount))((_, game, changes) => {
        renderBoard(game, changes, onMoveObserver, colors)
      }),
      child <-- solution
        .map {
          case None => div("Solver took too long to complete")
          case Some(value) => renderHint(value)
        }
        .startWith(div("Calculating solution..."))
    )
  }

}
