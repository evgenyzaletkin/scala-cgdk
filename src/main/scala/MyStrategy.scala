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
  var isHitting = false

  def move(self: Hockeyist, world: World, game: Game, move: Move): Unit = {
    self.state match {
      case HockeyistState.Swinging => actSwinging(self, move)
      case _ => actActive(self, world, game, move)
    }
  }

  def actSwinging(self: Hockeyist, move: Move) = {

    if (swingingTime  == 0) {
      move.action = ActionType.Strike
    }
    else {
      swingingTime -= 1
    }
  }

  def actActive(self: Hockeyist, world: World, game: Game, move: Move) = {
    if (world.puck.ownerPlayerId.contains(self.playerId)) {
      if (world.puck.ownerHockeyistId.contains(self.id)) {
        isHitting = false
        getEnemyGoalie(world) match {
          case None => {
            val opponent = world.opponentPlayer.get
            val netX = (opponent.netRight + opponent.netLeft) * 0.5
            val netY = (opponent.netTop + opponent.netBottom) * 0.5
            swing(move, self, self.angleTo(netX, netY), 0)
          }
          case _ => drivePuck (self, world, game, move)
        }
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
    val Some((netTop, netBottom, netX)) = for (
      opponentPlayer <- world.opponentPlayer
    ) yield (opponentPlayer.netTop, opponentPlayer.netBottom, 0.5 * (opponentPlayer.netFront + opponentPlayer.netBack))
    val distanceToGoal = game.goalNetHeight * math.tan(math.Pi / 3)
    val targetX = if (netX > (world.width / 2.0)) netX - distanceToGoal else netX + distanceToGoal
    val targetY = if (self.y > (world.height / 2.0)) netTop else netBottom
    if (!isHitting && !isInPosition(targetX, targetY, self, game)) {
      if (self.distanceTo(targetX, targetY) > 200)
        move.speedUp = 1.0D
      val angle = self.angleTo(targetX, targetY)
      move.turn = angle
    }
    else {
      isHitting = true
      val angleToStrike = if (self.y > world.height * 0.5) {
        self.angleTo(netX, netTop)
      } else {
        self.angleTo(netX, netBottom)
      }
      swing(move, self, angleToStrike, 5)
    }
  }

  def isInPosition(x: Double, y: Double, self: Hockeyist, game:Game) = {
    val isInXRange = self.x > x - 100 && self.x < x + 100
    val isInYRange = if (y <= game.goalNetTop) {
      self.y < y
    }
    else {
      self.y > y
    }
    isInXRange && isInYRange
  }


  def swing(move: Move, self: Hockeyist, angleToStrike: Double, swingingTime: Int) {

    move.turn = angleToStrike
    if (math.abs(angleToStrike) < StrikeAngle) {
      this.swingingTime = swingingTime
      move.action = ActionType.Swing
    }
  }



  def getEnemyGoalie(w: World) = {
    val hs = for (
      h <- w.hockeyists;
      p <- w.myPlayer
      if h.playerId == p.id && h.hokeyistType == HockeyistType.Goalie
    ) yield h
    hs match {
      case h +: hs1 => Some(h)
      case Vector() => None
    } 
  }

}