package com.clemenswinter.codecraftserver.controllers

import javax.inject.{Inject, Singleton}
import cwinter.codecraft.core.multiplayer._
import cwinter.codecraft.core.api._
import cwinter.codecraft.core.game._
import akka.pattern.ask
import akka.util.Timeout
import cwinter.codecraft.util.maths
import play.api.inject.ApplicationLifecycle

import scala.concurrent.duration._
import scala.language.postfixOps
import cwinter.codecraft.util.maths.{Rectangle, Vector2}

import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.util.Random

@Singleton
class MultiplayerServer @Inject()(lifecycle: ApplicationLifecycle) {
  println("Starting multiplayer server")
  var gameID = -1
  var games = Map.empty[Int, Game]
  var completedGames = Map.empty[(Int, Int), Observation]
  val actorRef = Server.start(maxGames = 20)
  implicit val timeout = Timeout(10 seconds)
  val server = Await
    .result(actorRef ? Server.ScrewThis, 11 seconds)
    .asInstanceOf[cwinter.codecraft.core.multiplayer.MultiplayerServer]
  val rng = new Random()

  def startGame(maxTicks: Option[Int], scriptedOpponent: Boolean, customMap: Option[MapSettings]): Integer =
    synchronized {
      val initialDrones = customMap.map(m => (m.player1Drones.size, m.player2Drones.size)).getOrElse((1, 1))
      val maxGameLength = maxTicks.getOrElse(3 * 60 * 60)
      val player1 = new PlayerController(maxGameLength, BluePlayer)
      var controllers: Seq[DroneControllerBase] =
        Seq.fill(initialDrones._1)(new PassiveDroneController(player1, Promise.successful(DoNothing)))
      var player2: Option[PlayerController] = None
      if (scriptedOpponent) {
        controllers ++= Seq.fill(initialDrones._2)(new AFK())
      } else {
        val p2 = new PlayerController(maxGameLength, OrangePlayer)
        player2 = Some(p2)
        controllers ++= Seq.fill(initialDrones._2)(
          new PassiveDroneController(p2, Promise.successful(DoNothing)))
      }
      val winCondition = Seq(DestroyAllEnemies, LargestFleet(maxGameLength))
      val map = customMap.map(
        m =>
          (
            new Rectangle(m.mapWidth / 2 - m.mapWidth,
                          m.mapWidth / 2,
                          m.mapHeight / 2 - m.mapHeight,
                          m.mapHeight / 2),
            m.player1Drones.map(_.toSpawn(BluePlayer)) ++ m.player2Drones.map(_.toSpawn(OrangePlayer)),
            // Seq.fill(5)((10, 25))
            Seq.fill(1)((2, 10))
        )
      )
      val simulator = server.startLocalGame(controllers, winCondition, map)
      gameID += 1
      val playerControllers = player2 match {
        case Some(p2) => Seq(player1, p2)
        case _ => Seq(player1)
      }
      games += gameID -> Game(simulator, playerControllers)
      println(f"Started game $gameID (${playerControllers.size} players). Running games: ${games.size}")
      gameID
    }

  val margin = 50
  def randomStartPos(size: Rectangle): Vector2 = Vector2(
    size.xMin + margin + (size.xMax - size.yMin + 2 * margin) * rng.nextFloat(),
    size.yMin + margin + (size.yMax - size.yMin + 2 * margin) * rng.nextFloat()
  )

  def observe(gameID: Int, playerID: Int): Observation = {
    val game = synchronized {
      if (completedGames.contains((gameID, playerID))) return completedGames((gameID, playerID))
      games(gameID)
    }
    val observation = game.externalPlayers(playerID).observe(game.simulator)
    synchronized {
      if (observation.winner.isDefined) {
        completedGames += (gameID, playerID) -> observation
        if (game.externalPlayers.size == 1 || completedGames.contains((gameID, 1 - playerID))) {
          games -= gameID
          println(f"Completed game $gameID. Running games: ${games.size}")
        }
      }
    }
    observation
  }

  def act(gameID: Int, playerID: Int, actions: Seq[Action]): Unit = synchronized {
    val game = games(gameID)
    if (game.simulator.winner.isDefined) return
    game.externalPlayers(playerID).act(actions)
  }

  def debugState(): Seq[GameDebugState] = {
    for ((id, game) <- games.toSeq) yield {
      GameDebugState(
        id,
        game.externalPlayers.head.observationsReady.isCompleted,
        game.simulator.winner.map(_.id),
        game.simulator.currentPhase.toString,
        game.externalPlayers.head.unsafe_observe(game.simulator)
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
  var nextAction: Promise[Action] = Promise()
) extends DroneController {
  override def onTick(): Unit = {
    if (missileCooldown == 0 && enemiesInSight.nonEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInMissileRange(closest)) fireMissilesAt(closest)
    }
    val action = try {
      Await.result(nextAction.future, Duration.Inf) // 60 seconds)
    } catch {
      case e: TimeoutException => {
        println("DROPPED ACTION")
        DoNothing
      }
    }

    for (spec <- action.buildDrone) {
      val droneSpec = DroneSpec(spec(0), spec(1), spec(2), spec(3), spec(4))
      if (droneSpec.resourceCost < storedResources) buildDrone(new PassiveDroneController(state), droneSpec)
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

    nextAction = Promise()
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

  override def onDroneEntersVision(drone: Drone): Unit = {
    if (drone.isEnemy) {
      state.enemyDrones += drone
    }
  }

  def setAction(action: Action): Unit = {
    nextAction.success(action)
  }

  override def metaController = Some(state)
}

class PlayerController(val maxGameLength: Int, val player: Player) extends MetaController {
  var alliedDrones: Seq[PassiveDroneController] = Seq.empty
  var enemyDrones: Set[Drone] = Set.empty
  @volatile var sim: Option[DroneWorldSimulator] = None
  @volatile var observationsReady: Promise[Unit] = Promise()
  var minerals = Set.empty[MineralCrystal]

  def observe(sim: DroneWorldSimulator): Observation = {
    Await.ready(observationsReady.future, Duration.Inf)
    unsafe_observe(sim)
  }

  def unsafe_observe(sim: DroneWorldSimulator): Observation = {
    val enemyPlayer = if (player == BluePlayer) OrangePlayer else BluePlayer
    Observation(
      sim.timestep,
      maxGameLength,
      sim.winner.map(_.id),
      for (d <- alliedDrones if !d.isDead)
        yield DroneObservation(d),
      (for {
        d <- enemyDrones
        if d.isVisible
      } yield DroneObservation(d)).toSeq,
      for {
        d <- sim.dronesFor(enemyPlayer)
      } yield DroneObservation(d),
      for (m <- minerals.toSeq if !m.harvested)
        yield MineralObservation(m.position.x, m.position.y, m.size),
      alliedDrones.toSeq
        .filter(!_.isDead)
        .map(score)
        .sum, // TODO: why doesn't dead drone get removed? (maybe one tick too late?)
      sim.dronesFor(enemyPlayer).map(score).sum
    )
  }

  def score(drone: Drone): Double = {
    drone.spec.resourceCost * (drone.hitpoints / drone.maxHitpoints.toDouble + 1.0)
  }

  def act(actions: Seq[Action]): Unit = {
    observationsReady = Promise()
    for ((d, i) <- alliedDrones.zipWithIndex) {
      val action = if (i < actions.size) actions(i) else DoNothing
      d.setAction(action)
    }
  }

  override def onTick(): Unit = {
    if (!observationsReady.isCompleted) observationsReady.success(())
  }

  override def gameOver(winner: Player): Unit = {
    if (!observationsReady.isCompleted) {
      observationsReady.success(())
    }
  }
}

case class Observation(
  timestep: Int,
  maxGameLength: Int,
  winner: Option[Int],
  alliedDrones: Seq[DroneObservation],
  visibleEnemyDrones: Seq[DroneObservation],
  allEnemyDrones: Seq[DroneObservation],
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
  isHarvesting: Boolean,
  isStunned: Boolean
)

object DroneObservation {
  def apply(d: Drone): DroneObservation = {
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
      d.isHarvesting,
      d.isStunned
    )
  }
}

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
  observationsReady: Boolean,
  winner: Option[Int],
  phase: String,
  observation: Observation
)

case class StartingDrone(
  xPos: Float,
  yPos: Float,
  resources: Int = 0,
  storageModules: Int = 0,
  missileBatteries: Int = 0,
  constructors: Int = 0,
  engines: Int = 0,
  shieldGenerators: Int = 0
) {
  def toSpawn(player: Player): Spawn = Spawn(
    DroneSpec(storageModules, missileBatteries, constructors, engines, shieldGenerators),
    Vector2(xPos, yPos),
    player,
    resources
  )
}

case class MapSettings(
  mapWidth: Int,
  mapHeight: Int,
  player1Drones: Seq[StartingDrone],
  player2Drones: Seq[StartingDrone]
)
