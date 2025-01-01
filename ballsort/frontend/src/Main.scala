package ballsort.frontend

import zio.*
import ballsort.frontend.services.SolverService

object Main extends ZIOAppDefault {
  def run = ZIO
    .serviceWithZIO[App](_.run)
    .provide(
      ZLayer.derive[App],
      ZLayer.derive[HomePage],
      ZLayer.derive[SolverService]
    )
}
