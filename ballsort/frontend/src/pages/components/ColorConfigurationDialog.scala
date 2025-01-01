package ballsort.frontend
import be.doeraene.webcomponents.ui5.configkeys.ButtonDesign
import be.doeraene.webcomponents.ui5.scaladsl.colour.Colour
import be.doeraene.webcomponents.ui5.{Button, ColourPalette, ColourPalettePopover, Dialog, Text}
import com.raquo.laminar.api.L.*
import foxxy.frontend.utils.*
import org.scalajs.dom.HTMLElement

import domain.Game

case class ColorConfigurationDialog() {
  def colorPallette = List(
    Colour.fromString("darkblue"),
    Colour.fromString("pink"),
    Colour.fromIntColour(0x444444),
    Colour(0, 200, 0),
    Colour.green,
    Colour.fromString("darkred"),
    Colour.yellow,
    Colour.blue,
    Colour.fromString("cyan"),
    Colour.orange,
    Colour.fromIntColour(0x5480e7),
    Colour.fromIntColour(0xff6699)
  ).map(colour => ColourPalette.item(_.value := colour))

  private def renderSmallBall(id: Int, colors: Var[Map[Int, String]]) = {
    val openPopoverBus: EventBus[HTMLElement] = new EventBus
    div(
      Text(id.toString),
      textAlign.center,
      alignContent.center,
      width        := "20px",
      height       := "20px",
      borderRadius := "50%",
      backgroundColor <-- colors.signal.map(_.getOrElse(id, "white")),
      margin       := "5px",
      idAttr       := s"ball-$id",
      inContext { el => onClick.mapTo(el.ref) --> openPopoverBus.writer },
      ColourPalettePopover(
        _.showAtOpenerIdFromEvents(openPopoverBus.events.map(_.id)),
        colorPallette,
        _.showRecentColours := true,
        _.showMoreColours   := true,
        _.showDefaultColour := true,
        _.defaultColour     := Colour.green,
        _.events.onItemClick.map(_.detail.scalaColour) --> { x => colors.update(_.updated(id, x.rgba)) },
        _.events.onClose --> Observer[Any](x => org.scalajs.dom.console.log(x))
      )
    )
  }

  def render(game: Signal[Option[Game]], colors: Var[Map[Int, String]]) = {
    val openDialogBus: EventBus[Boolean] = new EventBus
    div(
      Button(
        _.design := ButtonDesign.Emphasized,
        "Color configuration",
        _.events.onClick.mapTo(true) --> openDialogBus.writer
      ),
      Dialog(
        _.showFromEvents(openDialogBus.events.filter(identity).mapTo(())),
        _.closeFromEvents(openDialogBus.events.map(!_).filter(identity).mapTo(())),
        _.headerText   := "Color configuration", {
          hDiv(
            children <-- game
              .map(x => x.map(y => y.colorsCount).getOrElse(0))
              .map(x => {
                Range(0, x).map(i => renderSmallBall(i, colors))
              })
          )
        },
        _.slots.footer := div(
          div(flex := "1"),
          Button(
            _.design := ButtonDesign.Emphasized,
            "Save",
            _.events.onClick.mapTo(false) --> openDialogBus.writer
          )
        )
      )
    )
  }
}
