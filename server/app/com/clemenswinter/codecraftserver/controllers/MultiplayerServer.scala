package com.clemenswinter.codecraftserver.controllers

import javax.inject.{Inject, Singleton}

import cwinter.codecraft.core.multiplayer._
import cwinter.codecraft.core.api._
import cwinter.codecraft.core.game._
import akka.pattern.ask
import akka.util.Timeout
import play.api.inject.ApplicationLifecycle
import scala.concurrent.duration._
import scala.language.postfixOps

import scala.concurrent.Await
import scala.concurrent.{Promise, Future}

@Singleton
class MultiplayerServer @Inject()(lifecycle: ApplicationLifecycle) {
  println("Starting multiplayer server")
  var gameID = 0
  var games = Map.empty[Int, Game]
  var completedGames = Map.empty[Int, Observation]
  val actorRef = Server.start(maxGames = 20)
  implicit val timeout = Timeout(10 seconds)
  val server = Await.result(actorRef ? Server.ScrewThis, 11 seconds).asInstanceOf[cwinter.codecraft.core.multiplayer.MultiplayerServer]


  def startGame(): Integer = synchronized {
    val player1 = new PlayerController()
    val simulator = server.startLocalGame(new PassiveDroneController(player1), TheGameMaster.replicatorAI())
    games += gameID -> Game(simulator, Seq(player1))
    gameID += 1
    gameID - 1
  }


  def observe(gameID: Int, playerID: Int): Observation = {
    if (completedGames.contains(gameID)) return completedGames(gameID)
    val game = games(gameID)
    val observation = game.externalPlayers(playerID).observe(game.simulator)
    if (!observation.winner.isEmpty) {
      games -= gameID
      completedGames += gameID -> observation
    }
    observation
  }


  def act(gameID: Int, playerID: Int): Unit = {
    val game = games(gameID)
    if (!game.simulator.winner.isEmpty) return
    game.externalPlayers(playerID).act()
  }

  // it appears the class loader will not work during shutdown, so we need to get an instance of Server.Stop
  //       before the stop hook gets invoked
 val stop = Server.Stop
  lifecycle.addStopHook { () =>
    Future.successful(actorRef ! stop)
  }
}

case class Game(simulator: DroneWorldSimulator, externalPlayers: Seq[PlayerController])

class PassiveDroneController(
  var state: PlayerController,
  var nextAction: Promise[Action] = Promise()
) extends DroneController {
  nextAction.success(DoNothing)

  override def onTick(): Unit = {
    if (missileCooldown == 0 && !enemiesInSight.isEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInMissileRange(closest)) fireMissilesAt(closest)
    }
    val action = Await.result(nextAction.future, Duration.Inf)
    action match {
      case DoNothing =>
    }
    nextAction = Promise()
  }

  override def onSpawn(): Unit = {
    state.alliedDrones += this
  }

  override def onDeath(): Unit = {
    state.alliedDrones -= this
  }

  def setAction(action: Action): Unit = {
    nextAction.success(action)
  }

  override def metaController = Some(state)
}

class PlayerController(
  var alliedDrones: Set[PassiveDroneController] = Set.empty,
  var observationsReady: Promise[Unit] = Promise()
) extends MetaController {
  def observe(sim: DroneWorldSimulator): Observation = {
    Await.ready(observationsReady.future, Duration.Inf)

    Observation(
      sim.timestep,
      sim.winner.map(_.id),
      for (d <- alliedDrones.toSeq)
        yield DroneObservation(d.position.x, d.position.y, d.orientation.toFloat)
    )
  }

  def act(): Unit = {
    observationsReady = Promise()
    for (d <- alliedDrones) d.setAction(DoNothing)
  }

  override def onTick(): Unit = {
    if (!observationsReady.isCompleted) observationsReady.success(())
  }

  override def gameOver(winner: Player): Unit = {
    observationsReady.success(())
  }
}

case class Observation(
  timestep: Int,
  winner: Option[Int],
  alliedDrones: Seq[DroneObservation]
)

case class DroneObservation(
  xPos: Float,
  yPos: Float,
  orientation: Float
)

sealed trait Action
case object DoNothing extends Action

