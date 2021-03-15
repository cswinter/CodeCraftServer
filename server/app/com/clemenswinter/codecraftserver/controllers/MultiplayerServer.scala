package com.clemenswinter.codecraftserver.controllers

import javax.inject.{Inject, Singleton}
import cwinter.codecraft.core.multiplayer._
import cwinter.codecraft.core.api._
import cwinter.codecraft.core.game._
import akka.pattern.ask
import akka.util.Timeout
import play.api.inject.ApplicationLifecycle

import scala.concurrent.duration._
import scala.collection.mutable.Queue
import scala.language.postfixOps
import cwinter.codecraft.util.maths.{Rectangle, Vector2}

import scala.concurrent.{Await, Future, Promise, TimeoutException}
import scala.util.Random

@Singleton
class MultiplayerServer @Inject()(lifecycle: ApplicationLifecycle) {
  println("Starting multiplayer server")
  val maxCompletedGamesRetained = 1000
  var gameID = -1
  @volatile var games = Map.empty[Int, Game]
  var completedGamesQueue = Queue.empty[(Int, Int)]
  var completedGames = Map.empty[(Int, Int), Observation]
  val actorRef = Server.start(maxGames = 20)
  implicit val timeout = Timeout(10 seconds)
  val server = Await
    .result(actorRef ? Server.ScrewThis, 11 seconds)
    .asInstanceOf[cwinter.codecraft.core.multiplayer.MultiplayerServer]
  val rng = new Random()

  def startGame(maxTicks: Option[Int],
                scriptedOpponent: String,
                customMap: Option[MapSettings],
                rules: SpecialRules,
                allowHarvesting: Boolean,
                forceHarvesting: Boolean,
                randomizeIdle: Boolean): Integer = synchronized {
    val initialDrones = customMap.map(m => (m.player1Drones.size, m.player2Drones.size)).getOrElse((1, 1))
    val maxGameLength = maxTicks.getOrElse(3 * 60 * 60)
    val mapWidth = customMap.map(_.mapWidth).getOrElse(6000)
    val mapHeight = customMap.map(_.mapHeight).getOrElse(4000)
    val tileWidth = 400
    val player1 =
      new PlayerController(maxGameLength,
                           BluePlayer,
                           gameID + 1,
                           mapWidth,
                           mapHeight,
                           tileWidth,
                           rules,
                           allowHarvesting,
                           forceHarvesting,
                           randomizeIdle)
    var controllers: Seq[DroneControllerBase] =
      Seq.fill(initialDrones._1)(new PassiveDroneController(player1, Promise.successful(DoNothing)))
    var player2: Option[PlayerController] = None
    if (scriptedOpponent == "idle") {
      controllers ++= Seq.fill(initialDrones._2)(new AFK())
    } else if (scriptedOpponent == "destroyer") {
      controllers ++= Seq.fill(initialDrones._2)(TheGameMaster.destroyerAI())
    } else if (scriptedOpponent == "replicator") {
      controllers ++= Seq.fill(initialDrones._2)(TheGameMaster.replicatorAI())
    } else if (scriptedOpponent == "greedy_replicator") {
      controllers ++= Seq.fill(initialDrones._2)(TheGameMaster.replicatorAI(greedy = true))
    } else if (scriptedOpponent == "aggressive_replicator") {
      controllers ++= Seq.fill(initialDrones._2)(TheGameMaster.replicatorAI(aggressive = true))
    } else if (scriptedOpponent == "confident_replicator") {
      controllers ++= Seq.fill(initialDrones._2)(TheGameMaster.replicatorAI(confident = true))
    } else {
      assert(scriptedOpponent == "none")
      val p2 =
        new PlayerController(maxGameLength,
                             OrangePlayer,
                             gameID + 1,
                             mapWidth,
                             mapHeight,
                             tileWidth,
                             rules,
                             allowHarvesting,
                             forceHarvesting,
                             randomizeIdle)
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
          Left(m.minerals),
          m.symmetric
      )
    )
    val simulator = server.startLocalGame(controllers, winCondition, map, rules, 1.hour)
    gameID += 1
    val playerControllers = player2 match {
      case Some(p2) => Seq(player1, p2)
      case _ => Seq(player1)
    }
    games += gameID -> Game(simulator, playerControllers)
    gameID
  }

  val margin = 50

  def randomStartPos(size: Rectangle): Vector2 = Vector2(
    size.xMin + margin + (size.xMax - size.yMin + 2 * margin) * rng.nextFloat(),
    size.yMin + margin + (size.yMax - size.yMin + 2 * margin) * rng.nextFloat()
  )

  def startDemo() = {
    new Thread {
      override def run() {
        while (true) {
          _startDemo()
          Thread.sleep(2000)
        }
      }
    }.start()
    0
  }

  def _startDemo(): Int = synchronized {
    val (controllers, map) = Demo.create()
    val simulator = server.startLocalGame(
      controllers,
      Seq(DestroyAllEnemies, LargestFleet(3 * 60 * 60)),
      Some(map),
      SpecialRules.default,
      24.hours
    )
    gameID += 1
    games += gameID -> Game(simulator, Seq.empty)
    gameID
  }

  def observe(gameID: Int, playerID: Int, lastSeen: Boolean): Observation = {
    val key = (gameID, playerID)
    val game = synchronized {
      if (completedGames.contains(key)) return completedGames(key)
      games(gameID)
    }
    val observation = game.externalPlayers(playerID).observe(game.simulator, lastSeen)
    synchronized {
      if (observation.winner.isDefined || game.externalPlayers(playerID).stopped) {
        completedGames += key -> observation
        if (game.externalPlayers.size == 1 || completedGames.contains((gameID, 1 - playerID))) {
          games -= gameID
        }
        completedGamesQueue.enqueue(key)
        if (completedGamesQueue.size > maxCompletedGamesRetained) {
          val keyRemove = completedGamesQueue.dequeue()
          completedGames -= keyRemove
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

  def debugState(): (Seq[GameDebugState], Int) = {
    (for ((id, game) <- games.toSeq) yield {
      GameDebugState(
        id,
        game.externalPlayers.head.observationsReady.isCompleted,
        game.simulator.winner.map(_.id),
        game.simulator.currentPhase.toString,
        game.externalPlayers.head.unsafe_observe(game.simulator, true)
      )
    }, completedGames.size)
  }

  // it appears the class loader will not work during shutdown, so we need to get an instance of Server.Stop
  //       before the stop hook gets invoked
  private val stop = Server.Stop
  lifecycle.addStopHook { () =>
    Future.successful(actorRef ! stop)
  }
}

case class Game(simulator: DroneWorldSimulator, externalPlayers: Seq[PlayerController])

class AFK extends DroneController {
  override def onTick(): Unit = {
    if (missileCooldown == 0 && enemiesInSight.nonEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInMissileRange(closest)) fireMissilesAt(closest)
    }
    if (longRangeMissileChargeup == 0 && enemiesInSight.nonEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInLongRangeMissileRange(closest)) fireLongRangeMissilesAt(closest)
    }
  }
}

class PassiveDroneController(
  var state: PlayerController,
  @volatile var nextAction: Promise[Action] = Promise()
) extends DroneController {
  val id: Int = state.nextID()
  var lastAction: Action = Action(None, false, false, false, 0)
  var harvesting = Option.empty[Int]
  var currentlyHarvesting = Option.empty[MineralCrystal]
  var buildActionLocked = true

  override def onTick(): Unit = {
    state.updateTiles(position)

    if (!isHarvesting) {
      currentlyHarvesting = None
    }

    if (missileCooldown == 0 && enemiesInSight.nonEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInMissileRange(closest)) fireMissilesAt(closest)
    }
    if (longRangeMissileChargeup == 0 && enemiesInSight.nonEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInLongRangeMissileRange(closest)) fireLongRangeMissilesAt(closest)
    }
    val action = try {
      Log.debug(f"[${state.gameID}, ${state.player}] $id Waiting for action (=${this.hitpoints})")
      Await.result(nextAction.future, 1.hours)
    } catch {
      case e: TimeoutException => {
        println(f"TIMED OUT WAITING FOR ACTION, STOPPING GAME ${state.gameID}")
        state.stopped = true
        DoNothing
      }
    }
    Log.debug(f"[${state.gameID}, ${state.player}] $id Obtained action (hitpoints=${this.hitpoints})")

    for (spec <- action.buildDrone) {
      val droneSpec = DroneSpec(spec(0), spec(1), spec(2), spec(3), spec(4), spec(5))
      buildDrone(new PassiveDroneController(state), droneSpec)
    }

    if (constructors == 0 && storedResources > 0 && alliesInSight.nonEmpty) {
      for {
        ally <- alliesInSight
        if ally.storageModules > 0 && ally.constructors > 0
      } giveResourcesTo(ally)
    }

    if (state.allowHarvesting) {
      harvesting match {
        case Some(startingStore) if !isHarvesting || storedResources != startingStore =>
          harvesting = None
        case _ =>
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

    if (!isHarvesting && mineralsInSight.nonEmpty && storedResources < storageModules * 7 && state.allowHarvesting) {
      val closest =
        mineralsInSight.minBy(mc => (mc.position - position).lengthSquared)
      if (isInHarvestingRange(closest)) {
        harvest(closest)
        currentlyHarvesting = Some(closest)
      }
    }
    if (action.harvest) {
      val motherships = state.alliedDrones.filter(_.constructors > 0)
      if (motherships.nonEmpty) {
        val closestMS = motherships.minBy(x => (x.position - position).lengthSquared)
        moveTo(closestMS)
      }
    }

    Log.debug(
      f"[${state.gameID}, ${state.player}] $id Resetting action promise (hitpoints=${this.hitpoints})")
    nextAction = Promise()
  }

  override def onSpawn(): Unit = {
    state.alliedDrones :+= this
  }

  override def onDeath(): Unit = {
    state.alliedDrones = state.alliedDrones.filter(_ != this)
  }

  override def onMineralEntersVision(m: MineralCrystal): Unit = {
    // TODO: efficiency?
    if (!state.minerals.contains(m)) state.minerals :+= m
  }

  override def onDroneEntersVision(drone: Drone): Unit = {
    if (drone.isEnemy && !state.enemyDrones.contains(drone)) {
      // TODO: efficiency?
      state.enemyDrones :+= drone
    }
  }

  def setAction(action: Action): Unit = {
    // WORKAROUND, probably needed because of a race for drone spawn events.
    if (action.lockBuildAction) {
      buildActionLocked = true
      showText("Locking")
    }
    if (action.unlockBuildAction) {
      buildActionLocked = false
      showText("Unlocking")
    }
    lastAction = action
    if (!nextAction.isCompleted) {
      nextAction.success(action)
    }
  }

  override def metaController = Some(state)
}

class PlayerController(
  val maxGameLength: Int,
  val player: Player,
  val gameID: Int,
  val mapWidth: Int,
  val mapHeight: Int,
  val tileWidth: Int,
  val rules: SpecialRules,
  val allowHarvesting: Boolean,
  val forceHarvesting: Boolean,
  val randomizeIdle: Boolean
) extends MetaController {
  @volatile var alliedDrones = Seq.empty[PassiveDroneController]
  @volatile var enemyDrones = Seq.empty[Drone]
  @volatile var timeLastSeen = Map.empty[Drone, Int]
  @volatile var sim: Option[DroneWorldSimulator] = None
  @volatile var observationsReady: Promise[Unit] = Promise()
  @volatile var minerals = Seq.empty[MineralCrystal]
  @volatile var step0 = true
  private val rng = new Random()
  var time = 0
  var stopped = false
  var tiles: Array[Array[MapTile]] = Array.tabulate(
    (mapWidth + tileWidth - 1) / tileWidth,
    (mapHeight + tileWidth - 1) / tileWidth
  )(
    (x, y) =>
      new MapTile(x * tileWidth + tileWidth / 2 - mapWidth / 2,
                  y * tileWidth + tileWidth / 2 - mapHeight / 2,
                  0))
  var dronecount = 0
  var maxHitpoints = 0
  var minAlliedMSHealth = 0.0
  var minEnemyMSHealth = 0.0

  def observe(sim: DroneWorldSimulator, lastSeen: Boolean): Observation = {
    Log.debug(f"[$gameID, $player] Awaiting obs")

    try {
      Await.ready(observationsReady.future, 15.seconds)
    } catch {
      case e: TimeoutException => {
        println(s"OBSERVATION TIMED OUT, STOPPING GAME $gameID")
        stopped = true
        sim.terminate()
      }
    }
    Log.debug(f"[$gameID, $player] Obs ready")
    unsafe_observe(sim, lastSeen)
  }

  def unsafe_observe(sim: DroneWorldSimulator, lastSeen: Boolean): Observation = {
    val enemyPlayer = if (player == BluePlayer) OrangePlayer else BluePlayer
    if (maxHitpoints == 0) {
      maxHitpoints = math.max(
        (for (d <- sim.dronesFor(player)) yield d.maxHitpoints).max,
        (for (d <- sim.dronesFor(enemyPlayer)) yield d.maxHitpoints).max
      )
    }

    val alliedMS = for (d <- alliedDrones if d.constructors > 0) yield d.hitpoints / maxHitpoints.toDouble
    if (alliedMS.isEmpty) {
      minAlliedMSHealth = 0
    } else {
      minAlliedMSHealth = math.min(minAlliedMSHealth, alliedMS.min)
    }
    val enemyMS = for (d <- sim.dronesFor(enemyPlayer) if d.constructors > 0)
      yield d.hitpoints / maxHitpoints.toDouble
    if (enemyMS.isEmpty) {
      minEnemyMSHealth = 0
    } else {
      minEnemyMSHealth = math.min(minEnemyMSHealth, enemyMS.min)
    }

    enemyDrones = enemyDrones.filterNot(_.isDead)
    val timestep = sim.timestep
    Observation(
      sim.timestep,
      maxGameLength,
      if (stopped) Some(0) else sim.winner.map(_.id),
      for (d <- alliedDrones if !d.isDead)
        yield {
          val curentlyHarvesting = d.harvesting.exists(d.isHarvesting && _ == d.storedResources)
          DroneObservation(
            d,
            isEnemy = false,
            Some(d.lastAction),
            0,
            curentlyHarvesting || (d.isHarvesting && forceHarvesting),
            buildActionLocked = d.buildActionLocked
          )
        },
      (for {
        d <- enemyDrones
        if d.isVisible
      } yield {
        timeLastSeen += d -> sim.timestep
        DroneObservation(d,
                         isEnemy = true,
                         None,
                         sim.timestep - timeLastSeen.getOrElse(d, 0),
                         buildActionLocked = false)
      }) ++ (for {
        d <- enemyDrones
        if !d.isVisible && lastSeen
      } yield
        DroneObservation(d,
                         isEnemy = true,
                         None,
                         sim.timestep - timeLastSeen.getOrElse(d, 0),
                         buildActionLocked = true)),
      for {
        d <- sim.dronesFor(enemyPlayer)
      } yield
        DroneObservation(d,
                         isEnemy = true,
                         None,
                         sim.timestep - timeLastSeen.getOrElse(d, 0),
                         buildActionLocked = true),
      for (m <- minerals if !m.harvested)
        yield
          MineralObservation(m.position.x,
                             m.position.y,
                             m.size,
                             alliedDrones.exists(_.currentlyHarvesting.contains(m))),
      alliedDrones
        .filter(!_.isDead)
        .map(score)
        .sum, // TODO: why doesn't dead drone get removed? (maybe one tick too late?)
      sim.dronesFor(enemyPlayer).map(score).sum,
      minAlliedMSHealth,
      minEnemyMSHealth,
      sim.config.worldSize.height,
      sim.config.worldSize.width,
      tiles.flatMap(_.toSeq),
      rules
    )
  }

  def score(drone: Drone): Double = {
    drone.spec.resourceCost * (drone.hitpoints / drone.maxHitpoints.toDouble + 1.0)
  }

  def act(actions: Seq[Action]): Unit = {
    observationsReady = Promise()
    for ((d, i) <- alliedDrones.zipWithIndex) {
      Log.debug(f"[$gameID, $player] set action on ${d.id}")
      val action =
        if (i < actions.size) actions(i)
        else if (randomizeIdle) Action(None, rng.nextBoolean(), true, true, rng.nextInt(3) - 1)
        else DoNothing
      d.setAction(action)
    }
  }

  override def onTick(): Unit = {
    if (step0) {
      step0 = false
    } else if (!observationsReady.isCompleted) {
      time += 1
      Log.debug(f"[$gameID, $player] marking observation ready")
      observationsReady.success(())
    }
  }

  override def gameOver(winner: Player): Unit = {
    if (!observationsReady.isCompleted) {
      observationsReady.success(())
    }
  }

  def nextID(): Int = { dronecount += 1; dronecount }

  def updateTiles(pos: Vector2): Unit = {
    var x = ((pos.x + mapWidth / 2) / tileWidth).toInt
    var y = ((pos.y + mapHeight / 2) / tileWidth).toInt

    if (x < 0) x = 0
    if (x >= tiles.length) x = tiles.length - 1
    if (y < 0) y = 0
    if (y >= tiles(0).length) y = tiles(0).length - 1

    val tile = tiles(x)(y)

    // assert(tile.centerX - tileWidth / 2 <= pos.x)
    // assert(pos.x <= tile.centerX + tileWidth / 2)
    // assert(tile.centerY - tileWidth / 2 <= pos.y)
    // assert(pos.y <= tile.centerY + tileWidth / 2)

    tile.lastVisitedTime = time
  }
}

case class MapTile(
  centerX: Int,
  centerY: Int,
  var lastVisitedTime: Int
)

sealed case class Observation(
  timestep: Int,
  maxGameLength: Int,
  winner: Option[Int],
  alliedDrones: Seq[DroneObservation],
  visibleEnemyDrones: Seq[DroneObservation],
  allEnemyDrones: Seq[DroneObservation],
  minerals: Seq[MineralObservation],
  alliedScore: Double,
  enemyScore: Double,
  minAlliedMSHealth: Double,
  minEnemyMSHealth: Double,
  mapHeight: Double,
  mapWidth: Double,
  tiles: Seq[MapTile],
  rules: SpecialRules
)

sealed case class DroneObservation(
  xPos: Float,
  yPos: Float,
  orientation: Float,
  size: Int,
  storageModules: Int,
  missileBatteries: Int,
  constructors: Int,
  engines: Int,
  shieldGenerators: Int,
  longRangeMissiles: Int,
  hitpoints: Int,
  storedResources: Int,
  isConstructing: Boolean,
  isHarvesting: Boolean,
  isStunned: Boolean,
  isEnemy: Boolean,
  lastAction: Option[Action],
  missileCooldown: Int,
  longRangeMissileChargeup: Int,
  timeSinceVisible: Int,
  isVisible: Boolean,
  buildActionLocked: Boolean,
  harvestLock: Boolean,
  availableEnergy: Int,
  requiredEnergy: Int,
  constructionSpec: Option[Seq[Int]]
)

object DroneObservation {
  def apply(d: Drone,
            isEnemy: Boolean,
            lastAction: Option[Action],
            timeSinceVisible: Int,
            currentlyHarvesting: Boolean = false,
            buildActionLocked: Boolean): DroneObservation = {
    DroneObservation(
      if (d.isVisible) d.position.x else d.lastKnownPosition.x,
      if (d.isVisible) d.position.y else d.lastKnownPosition.y,
      if (d.isVisible) d.orientation.toFloat else d.lastKnownOrientation.toFloat,
      d.spec.moduleCount,
      d.storageModules,
      d.missileBatteries,
      d.constructors,
      d.engines,
      d.shieldGenerators,
      d.spec.longRangeMissiles,
      if (d.isVisible) d.hitpoints else d.maxHitpoints,
      if (d.isVisible) d.storedResources else 0,
      if (d.isVisible) d.isConstructing else false,
      if (d.isVisible) d.isHarvesting else false,
      if (d.isVisible) d.isStunned else false,
      isEnemy,
      lastAction,
      if (d.isVisible && d.missileBatteries > 0) d.missileCooldown else GameConstants.MissileCooldown,
      if (d.isVisible && d.longRangeMissileChargeup > 0) d.longRangeMissileChargeup else 0,
      timeSinceVisible,
      d.isVisible,
      buildActionLocked = buildActionLocked,
      harvestLock = currentlyHarvesting,
      availableEnergy = if (d.isVisible) d.availableEnergy else 0,
      requiredEnergy =
        if (d.isVisible) d.requiredEnergy.getOrElse(GameConstants.DroneConstructionTime * 50)
        else GameConstants.DroneConstructionTime * 50,
      constructionSpec = if (d.isVisible) d.constructionSpec.map(droneSpecToSeq) else None
    )
  }

  def droneSpecToSeq(s: DroneSpec): Seq[Int] = Seq(
    s.storageModules,
    s.missileBatteries,
    s.constructors,
    s.engines,
    s.shieldGenerators,
    s.longRangeMissiles
  )
}

sealed case class MineralObservation(
  xPos: Float,
  yPos: Float,
  size: Int,
  claimed: Boolean
)

case class Action(
  buildDrone: Option[Seq[Int]],
  move: Boolean,
  harvest: Boolean,
  transfer: Boolean,
  turn: Int /* -1, 0, 1 */,
  unlockBuildAction: Boolean = false,
  lockBuildAction: Boolean = false
) {
  def toInt: Int = {
    if (buildDrone.isDefined) {
      7
    } else {
      (if (move) 0 else 3) + turn + 1
    }
  }
}

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
  shieldGenerators: Int = 0,
  longRangeMissiles: Int = 0
) {
  def toSpawn(player: Player): Spawn = Spawn(
    DroneSpec(storageModules, missileBatteries, constructors, engines, shieldGenerators, longRangeMissiles),
    Vector2(xPos, yPos),
    player,
    resources
  )
}

case class GameSettings(
  map: Option[MapSettings],
  costModifiers: Seq[(Seq[Int], Double)]
)

case class MapSettings(
  mapWidth: Int,
  mapHeight: Int,
  player1Drones: Seq[StartingDrone],
  player2Drones: Seq[StartingDrone],
  minerals: Seq[(Int, Int)],
  symmetric: Boolean
)

object Log {
  val enableDebugLog = false

  def debug(msg: String) = if (enableDebugLog) println(msg) else Unit
}
