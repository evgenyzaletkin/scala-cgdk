import model.{Hockeyist, World, Game, Move, ActionType, HockeyistType, HockeyistState, Puck}
import MyStrategy.{StrikeAngle, getNearestOpponent}

object MyStrategy {
  private def getNearestOpponent(x: Double, y: Double, world: World): Option[Hockeyist] = {
    val hockeyists = world.hockeyists.collect({
      case hockeyist if !hockeyist.teammate && hockeyist.hokeyistType != HockeyistType.Goalie
        && hockeyist.state != HockeyistState.KnockedDown && hockeyist.state != HockeyistState.Resting
      => hockeyist
    })

    if (hockeyists.isEmpty) {
      None
    } else {
      Some(hockeyists.minBy {
        hockeyist => math.hypot(x - hockeyist.x, y - hockeyist.y)
      })
    }
  }

  private final val StrikeAngle = math.Pi / 180.0D
}

class MyStrategy extends Strategy {
  def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
    self.state match {
      case HockeyistState.Swinging => move.action = ActionType.Strike
      case _ => actActive(self, world, game, move)
    }
  }

  def actActive(self: Hockeyist, world: World, game: Game, move: Move) = {
    if (world.puck.ownerPlayerId.contains(self.playerId)) {
      if (world.puck.ownerHockeyistId.contains(self.id)) {
        drivePuck(self, world, game, move)
      }
      else {
        strikeNearestOpponent(self, world, game, move)
      }
    }
    else {
      moveToPuck(self, world.puck, move)
    }
  }


  private def strikeNearestOpponent(self: Hockeyist, world: World, game: Game, move: Move) {
    for (nearestOpponent <- getNearestOpponent(self.x, self.y, world)) {
      if (self.distanceTo(nearestOpponent) > game.stickLength) {
        move.speedUp = 1.0D
        move.turn = self.angleTo(nearestOpponent)
      }
      if (math.abs(self.angleTo(nearestOpponent)) < 0.5D * game.stickSector) {
        move.action = ActionType.Strike
      }
    }
  }

  private def moveToPuck(self: Hockeyist, puck: Puck, move: Move) {
    move.speedUp = 1.0D
    move.turn = self.angleTo(puck)
    move.action = ActionType.TakePuck
  }

  private def drivePuck(self: Hockeyist, world: World, game: Game, move: Move) {
    val Some((netX, netY)) = for {
      opponentPlayer <- world.opponentPlayer
      netX = 0.5D * (opponentPlayer.netBack + opponentPlayer.netFront)
      netY = {
        val ny = 0.5D * (opponentPlayer.netBottom + opponentPlayer.netTop)
        ny + ((if (self.y < ny) 0.5D else -0.5D) * game.goalNetHeight)
      }
    } yield (netX, netY)

    val angleToNet = self.angleTo(netX, netY)
    move.turn = angleToNet
    if (math.abs(angleToNet) < StrikeAngle) {
      move.action = ActionType.Swing
    }
  }
}