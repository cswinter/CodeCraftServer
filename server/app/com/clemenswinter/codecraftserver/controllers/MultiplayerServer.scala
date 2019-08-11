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
import java.util.concurrent.{BlockingQueue, ArrayBlockingQueue}

import scala.concurrent.{Await, Promise, Future, TimeoutException}

@Singleton
class MultiplayerServer @Inject()(lifecycle: ApplicationLifecycle) {
  println("Starting multiplayer server")
  var gameID = -1
  var games = Map.empty[Int, Game]
  var completedGames = Map.empty[Int, Observation]
  val actorRef = Server.start(maxGames = 20)
  implicit val timeout: Timeout = Timeout(10 seconds)
  val server = Await
    .result(actorRef ? Server.ScrewThis, 11 seconds)
    .asInstanceOf[cwinter.codecraft.core.multiplayer.MultiplayerServer]

  def startGame(maxTicks: Option[Int], actionDelay: Int): Integer = synchronized {
    val player1 = new PlayerController(actionDelay)
    val simulator = server.startLocalGame(
      new PassiveDroneController(player1, actionDelay),
      //TheGameMaster.replicatorAI())
      //TheGameMaster.level1AI(),
      new AFK(),
      Seq(DestroyEnemyMotherships, LargestFleet(maxTicks.getOrElse(3 * 60 * 60)))
    )
    player1.sim = Some(simulator)
    gameID += 1
    games += gameID -> Game(simulator, Seq(player1))
    gameID
  }

  def observe(gameID: Int, playerID: Int): Observation = {
    val game = synchronized {
      if (completedGames.contains(gameID)) return completedGames(gameID)
      games(gameID)
    }
    val observation = game.externalPlayers(playerID).observe(game.simulator)
    synchronized {
      if (observation.winner.isDefined) {
        games -= gameID
        completedGames += gameID -> observation
      }
    }
    observation
  }

  def act(gameID: Int, playerID: Int, action: Action): Unit = synchronized {
    val game = games(gameID)
    if (game.simulator.winner.isDefined) return
    game.externalPlayers(playerID).act(action)
  }

  def debugState: Seq[GameDebugState] = {
    for ((id, game) <- games.toSeq) yield {
      GameDebugState(
        id,
        game.externalPlayers.head.observations.size(),
        game.simulator.winner.map(_.id),
        game.simulator.currentPhase.toString,
        game.externalPlayers.head.unsafe_observe()
      )
    }
  }

  // it appears the class loader will not work during shutdown, so we need to get an instance of Server.Stop
  //       before the stop hook gets invoked
  private val stop = Server.Stop
  lifecycle.addStopHook { () =>
    Future.successful(actorRef ! stop)
  }
}

case class Game(simulator: DroneWorldSimulator, externalPlayers: Seq[PlayerController])

class AFK extends DroneController

class PassiveDroneController(
  var state: PlayerController,
  val actionDelay: Int
) extends DroneController {
  var nextActions: BlockingQueue[Action] = new ArrayBlockingQueue(actionDelay + 2)
  for (_ <- 0 until actionDelay + 1) nextActions.add(DoNothing)

  override def onTick(): Unit = {
    if (missileCooldown == 0 && enemiesInSight.nonEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInMissileRange(closest)) fireMissilesAt(closest)
    }
    val action = nextActions.take()
    for (spec <- action.buildDrone) {
      val droneSpec = DroneSpec(spec(0), spec(1), spec(2), spec(3), spec(4))
      if (droneSpec.resourceCost < storedResources) {
        buildDrone(new PassiveDroneController(state, actionDelay), droneSpec)
      }
    }

    if (action.move && action.turn == 0) {
      moveInDirection(orientation)
    } else if (action.move && action.turn == -1) {
      moveInDirection(orientation - 0.249)
    } else if (action.move && action.turn == 1) {
      moveInDirection(orientation + 0.249)
    } else if (action.turn == -1) {
      moveInDirection(orientation - 2)
    } else if (action.turn == 1) {
      moveInDirection(orientation + 2)
    } else {
      halt()
    }

    if (!isHarvesting && action.harvest && mineralsInSight.nonEmpty) {
      val closest =
        mineralsInSight.minBy(mc => (mc.position - position).lengthSquared)
      if (isInHarvestingRange(closest)) harvest(closest)
    }

    if (action.transfer && storedResources > 0) {
      val closest =
        alliesInSight.minBy(ally => (ally.position - position).lengthSquared)
      giveResourcesTo(closest)
    }
  }

  override def onSpawn(): Unit = {
    state.alliedDrones :+= this
  }

  override def onDeath(): Unit = {
    state.alliedDrones = state.alliedDrones.filter(_ != this)
  }

  override def onMineralEntersVision(m: MineralCrystal): Unit = {
    state.minerals += m
  }

  def setAction(action: Action): Unit = {
    nextActions.put(action)
  }

  override def metaController = Some(state)
}

class PlayerController(val actionDelay: Int) extends MetaController {
  var alliedDrones: Seq[PassiveDroneController] = Seq.empty
  @volatile var sim: Option[DroneWorldSimulator] = None
  var minerals = Set.empty[MineralCrystal]
  var observations: BlockingQueue[Observation] = new ArrayBlockingQueue[Observation](actionDelay + 2)
  private var firstTick = true

  def observe(sim: DroneWorldSimulator): Observation = observations.take()

  def unsafe_observe(): Observation = {
    while (this.sim.isEmpty) {}
    val s = sim.get
    Observation(
      s.timestep,
      s.winner.map(_.id),
      for (d <- alliedDrones)
        yield
          DroneObservation(
            d.position.x,
            d.position.y,
            d.orientation.toFloat,
            d.spec.moduleCount,
            d.storageModules,
            d.missileBatteries,
            d.constructors,
            d.engines,
            d.shieldGenerators,
            d.hitpoints,
            d.storedResources,
            d.isConstructing,
            d.isHarvesting
          ),
      for (m <- minerals.toSeq if !m.harvested)
        yield MineralObservation(m.position.x, m.position.y, m.size),
      alliedDrones.toSeq.map(score).sum,
      0.0
    )
  }

  def score(drone: Drone): Double =
    drone.spec.resourceCost * (drone.hitpoints / drone.maxHitpoints.toDouble + 1.0)

  def act(action: Action): Unit = {
    for (d <- alliedDrones) {
      if (d.constructors > 0) d.setAction(action)
      else d.setAction(DoNothing)
    }
  }

  override def onTick(): Unit = {
    if (firstTick) {
      firstTick = false
      return
    }
    observations.add(unsafe_observe())
  }

  override def gameOver(winner: Player): Unit = observations.add(unsafe_observe())
}

case class Observation(
  timestep: Int,
  winner: Option[Int],
  alliedDrones: Seq[DroneObservation],
  minerals: Seq[MineralObservation],
  alliedScore: Double,
  enemyScore: Double
)

case class DroneObservation(
  xPos: Float,
  yPos: Float,
  orientation: Float,
  size: Int,
  storageModules: Int,
  missileBatteries: Int,
  constructors: Int,
  engines: Int,
  shieldGenerators: Int,
  hitpoints: Int,
  storedResources: Int,
  isConstructing: Boolean,
  isHarvesting: Boolean
)

case class MineralObservation(
  xPos: Float,
  yPos: Float,
  size: Int
)

case class Action(
  buildDrone: Option[Seq[Int]],
  move: Boolean,
  harvest: Boolean,
  transfer: Boolean,
  turn: Int /* -1, 0, 1 */
)

object DoNothing
    extends Action(
      buildDrone = None,
      move = false,
      harvest = false,
      transfer = false,
      turn = 0
    )

case class GameDebugState(
  id: Int,
  observationsReady: Int,
  winner: Option[Int],
  phase: String,
  observation: Observation
)
