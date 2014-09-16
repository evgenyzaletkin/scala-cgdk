import model.{Hockeyist, World, Game, Move, ActionType, HockeyistType, HockeyistState, Puck}
import MyStrategy.{StrikeAngle, getNearestOpponent}

object MyStrategy {
  private def getNearestOpponent(x: Double, y: Double, world: World) = {
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

  var swingingTime = 0

  def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
    self.state match {
      case HockeyistState.Swinging => actSwinging(self, move)
      case _ => actActive(self, world, game, move)
    }
  }

  def actSwinging(self: Hockeyist, move: Move) = {

    if (swingingTime >= 9) {
      swingingTime = 0
      move.action = ActionType.Strike
    }
    else {
      swingingTime += 1
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


  def strikeNearestOpponent(self: Hockeyist, world: World, game: Game, move: Move) {
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

  def moveToPuck(self: Hockeyist, puck: Puck, move: Move) {
    move.speedUp = 1.0D
    move.turn = self.angleTo(puck)
    move.action = ActionType.TakePuck
  }

  def drivePuck(self: Hockeyist, world: World, game: Game, move: Move) {
    val Some((netX, netY)) = for {
      opponentPlayer <- world.opponentPlayer

      netX = 0.5D * (opponentPlayer.netBack + opponentPlayer.netFront)
      netY = {
        val ny = 0.5D * (opponentPlayer.netBottom + opponentPlayer.netTop)
        ny + ((if (self.y < ny) 0.49D else -0.49D) * game.goalNetHeight)
      }
    } yield (netX, netY)

    val angleToNet = self.angleTo(netX, netY)
    move.turn = angleToNet
    if (math.abs(angleToNet) < StrikeAngle) {
      move.action = ActionType.Swing
    }
  }

  def moveToEdge(self: Hockeyist, world: World, game: Game, move: Move) = {
    val Some((netTop, netBottom, netX)) = for (
      opponentPlayer <- world.opponentPlayer
    ) yield (opponentPlayer.netTop, opponentPlayer.netBottom, 0.5 * (opponentPlayer.netFront + opponentPlayer.netBack))
    if (self.y >= netTop) {
      //strike to bottom edge
      val angle = self.angleTo(netX, netBottom)
      performStrike(move, angle)
    }
    else if (self.y <= netBottom) {
      val angle = self.angleTo(netX, netTop)
      performStrike(move, angle)
    }
    else {
      val distanceToTop = netTop - self.y
      val distanceToBottom = self.y - netBottom
      if (distanceToTop > distanceToBottom) {

      }

    }
  }
  def performStrike(move: Move, angleToStrike: Double) {
    move.turn = angleToStrike
    if (math.abs(angleToStrike) < StrikeAngle)
      move.action = ActionType.Swing
  }



  def getEnemyGoalie(w: World) = {
    val hs = for (
      h <- w.hockeyists;
      p <- w.myPlayer
      if h.playerId == p.id && h.hokeyistType == HockeyistType.Goalie
    ) yield h
    hs match {
      case h :: hs1 => Some(h)
      case Nil => None
    } 
  }

}