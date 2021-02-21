package com.clemenswinter.codecraftserver.controllers

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.nio.FloatBuffer

import javax.inject._
import betterviews._
import cwinter.codecraft.core.multiplayer.{DetailedStatus, Server}

import scala.concurrent.duration._
import scala.language.postfixOps
import akka.pattern.ask
import akka.util.Timeout
import cwinter.codecraft.core.api.DroneSpec
import cwinter.codecraft.core.game.SpecialRules
import cwinter.codecraft.util.maths.Vector2
import play.api.mvc._

import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import upickle.default._

case class ObsConfig(
  allies: Int,
  drones: Int,
  minerals: Int,
  globalDrones: Int,
  tiles: Int,
  relativePositions: Boolean,
  extraBuildActions: Seq[Seq[Int]], // Number of each modules for custom build actions
  obsLastAction: Boolean,
  lastSeen: Boolean,
  isVisible: Boolean,
  abstime: Boolean,
  mapSize: Boolean,
  ruleMsdm: Boolean,
  ruleCosts: Boolean,
  harvestAction: Boolean,
  mineralAvailable: Boolean,
  lockBuildAction: Boolean,
  distanceToWall: Boolean,
  unitCount: Boolean,
  enforceUnitCap: Boolean
) {
  def rules: Int = (if (ruleMsdm) 1 else 0) + (if (ruleCosts) extraBuildActions.length + 1 else 0)
}

@Singleton
class Application @Inject()(
  cc: ControllerComponents,
  val multiplayerServer: MultiplayerServer
) extends AbstractController(cc) {

  def index = Action {
    Ok(Index()).as("text/html")
  }

  def observe(autorestart: Boolean, autozoom: Boolean) = Action {
    Ok(Observe(autorestart, autozoom)).as("text/html")
  }

  def startGame(
    maxTicks: Option[Int],
    actionDelay: Int,
    scriptedOpponent: String,
    mothershipDamageMultiplier: Double,
    allowHarvesting: Boolean,
    forceHarvesting: Boolean,
    randomizeIdle: Boolean
  ) =
    Action { implicit request =>
      val body = request.body.asJson.get.toString
      val config = read[GameSettings](body)
      val rules = SpecialRules(
        mothershipDamageMultiplier,
        (for ((Seq(storageModules, missileBatteries, constructors, engines, shieldGenerators), modifier) <- config.costModifiers)
          yield
            (DroneSpec(storageModules, missileBatteries, constructors, engines, shieldGenerators), modifier)).toMap
      )
      val id = multiplayerServer.startGame(maxTicks,
                                           scriptedOpponent,
                                           config.map,
                                           rules,
                                           allowHarvesting,
                                           forceHarvesting,
                                           randomizeIdle)
      Ok(f"""{"id": $id}""").as("application/json")
    }

  def startDemo() = Action {
    val id = multiplayerServer.startDemo()
    Ok(f"""{"id": $id}""").as("application/json")
  }

  def act(gameID: Int, playerID: Int) = Action { implicit request =>
    val action = read[Action](request.body.asJson.get.toString)
    multiplayerServer.act(gameID, playerID, Seq(action))
    Ok("success").as("application/json")
  }

  def playerState(gameID: Int, playerID: Int) = Action {
    val payload = multiplayerServer.observe(gameID, playerID, lastSeen = true)
    Ok(write(payload)).as("application/json")
  }

  def batchAct() = Action { implicit request =>
    val actions = read[Map[String, Seq[Action]]](request.body.asJson.get.toString)
    for ((gameID, action) <- actions) {
      val (gid, pid) = if (gameID.contains('.')) {
        val parts = gameID.split('.')
        (parts(0).toInt, parts(1).toInt)
      } else {
        (gameID.toInt, 0)
      }
      multiplayerServer.act(gid, pid, action)
    }
    Ok("success").as("application/json")
  }

  def batchPlayerState(json: Boolean,
                       allies: Int,
                       drones: Int,
                       minerals: Int,
                       globalDrones: Int,
                       tiles: Int,
                       relativePositions: Boolean,
                       v2: Boolean,
                       buildActions: Seq[Int],
                       obsLastAction: Boolean,
                       lastSeen: Boolean,
                       isVisible: Boolean,
                       abstime: Boolean,
                       mapSize: Boolean,
                       ruleMsdm: Boolean,
                       ruleCosts: Boolean,
                       harvestAction: Boolean,
                       mineralClaims: Boolean,
                       lockBuildAction: Boolean,
                       distanceToWall: Boolean,
                       unitCount: Boolean,
                       enforceUnitCap: Boolean) = Action { implicit request =>
    val (games, buildActions) = read[(Seq[(Int, Int)], Seq[Seq[Int]])](request.body.asJson.get.toString)
    val obsConfig =
      ObsConfig(
        allies,
        drones,
        minerals,
        globalDrones,
        tiles,
        relativePositions,
        buildActions,
        obsLastAction,
        lastSeen,
        isVisible,
        abstime,
        mapSize,
        ruleMsdm,
        ruleCosts,
        harvestAction,
        mineralClaims,
        lockBuildAction,
        distanceToWall,
        unitCount,
        enforceUnitCap
      )
    val payload: Seq[Observation] = for ((gameID, playerID) <- games)
      yield multiplayerServer.observe(gameID, playerID, lastSeen)
    if (json) {
      Ok(write(payload)).as("application/json")
    } else {
      if (v2) {
        val serializer = new ObsSerializer(payload, obsConfig)
        Ok(serializer.serialize()).as("application/octet-stream")
      } else {
        BadRequest("v1 obs is deprecated").as("text/html")
      }
    }
  }

  def mpssJson = Action.async { implicit request =>
    val maxGameStats = request
      .getQueryString("maxgames")
      .fold(250)(x => Try { x.toInt }.getOrElse(250))
    implicit val timeout = Timeout(1 seconds)
    val serverStatusFuture = multiplayerServer.actorRef ? Server.GetDetailedStatus
    for {
      untypedStatus <- serverStatusFuture
      status = untypedStatus.asInstanceOf[DetailedStatus]
      lessGames = status.games.sortBy(-_.startTimestamp).take(maxGameStats)
    } yield Ok(write(status.copy(games = lessGames))).as("application/json")
  }

  def debugState = Action {
    Ok(write(multiplayerServer.debugState()._1)).as("application/json")
  }
}

class ObsSerializer(obs: Seq[Observation], obsConfig: ObsConfig) {
  assert(!obsConfig.relativePositions)
  private val globalFeat = 2 + (if (obsConfig.mapSize) 2 else 0) + (if (obsConfig.abstime) 2 else 0) +
    obsConfig.rules + (if (obsConfig.unitCount) 1 else 0)
  private val extraFeat = 5 // Unobserved features such as score, winner, ms health
  private val allyDroneFeat = 15 + (if (obsConfig.obsLastAction) 8 else 0) +
    (if (obsConfig.lastSeen) 2 else 0) + (if (obsConfig.isVisible) 1 else 0) +
    (if (obsConfig.lockBuildAction) 1 else 0) + (if (obsConfig.distanceToWall) 5 else 0)
  private val enemyDroneFeat = 15 + (if (obsConfig.lastSeen) 2 else 0) +
    (if (obsConfig.isVisible) 1 else 0) +
    (if (obsConfig.lockBuildAction) 1 else 0) + (if (obsConfig.distanceToWall) 5 else 0)
  private val mineralFeat = if (obsConfig.mineralAvailable) 4 else 3
  private val tileFeat = 4
  private val enemies = obsConfig.drones - obsConfig.allies
  private val totalObjectFeat =
    allyDroneFeat * obsConfig.allies +
      2 * enemyDroneFeat * enemies +
      mineralFeat * obsConfig.minerals +
      tileFeat * obsConfig.tiles
  private val naction = 8 + obsConfig.extraBuildActions.size + (if (obsConfig.lockBuildAction) 2 else 0)
  private val actionMaskFeat = obsConfig.allies * naction
  private val size = obs.length * (globalFeat + totalObjectFeat + extraFeat + actionMaskFeat)
  private val bb: ByteBuffer = ByteBuffer.allocate(4 * size)
  bb.order(ByteOrder.nativeOrder)

  def serialize(): Array[Byte] = {
    obs.foreach(serializeObjects)
    obs.foreach(serializeScores)
    obs.foreach(serializeActionMasks)

    assert(bb.position() == 4 * size, f"Expected ${4 * size} elements, actual: ${bb.position()}")
    bb.array()
  }

  def serializeObjects(ob: Observation): Unit = {
    // Global features
    bb.putFloat(ob.timestep.toFloat / ob.maxGameLength)
    bb.putFloat(ob.alliedScore.toFloat)
    if (obsConfig.mapSize) {
      bb.putFloat(ob.mapHeight.toFloat)
      bb.putFloat(ob.mapWidth.toFloat)
    }
    if (obsConfig.abstime) {
      bb.putFloat(ob.timestep)
      bb.putFloat(ob.maxGameLength - ob.timestep)
    }
    if (obsConfig.ruleMsdm) {
      bb.putFloat(ob.rules.mothershipDamageMultiplier.toFloat)
    }
    if (obsConfig.ruleCosts) {
      assert(ob.rules.unitCostModifiers.size == obsConfig.extraBuildActions.size + 1)
      val sorted: Seq[(DroneSpec, Double)] =
        ob.rules.unitCostModifiers.toSeq.sortBy(x => DroneSpec.unapply(x._1))
      for ((_, m) <- sorted) {
        bb.putFloat(math.log(m).toFloat)
      }
    }
    if (obsConfig.unitCount) {
      bb.putFloat(math.min(ob.alliedDrones.size, obsConfig.allies))
    }

    // Allies
    ob.alliedDrones.take(obsConfig.allies).foreach(serializeDrone(ob, _))
    val apadding = (obsConfig.allies - ob.alliedDrones.size) * allyDroneFeat
    for (_ <- 0 until apadding) bb.putFloat(0.0f)

    // Enemies
    val nEnemy = obsConfig.drones - obsConfig.allies
    ob.visibleEnemyDrones.take(nEnemy).foreach(serializeDrone(ob, _))
    val epadding = (nEnemy - ob.visibleEnemyDrones.size) * enemyDroneFeat
    for (_ <- 0 until epadding) bb.putFloat(0.0f)

    // Minerals
    ob.minerals.take(obsConfig.minerals).foreach(serializeMineral)
    val mpadding = (obsConfig.minerals - ob.minerals.size) * mineralFeat
    for (_ <- 0 until mpadding) bb.putFloat(0.0f)

    // Tiles
    val tiles = ob.tiles
      .sortBy(t => (t.lastVisitedTime, t.hashCode))
      .take(obsConfig.tiles)
    tiles.foreach(serializeTile(ob.timestep, ob.maxGameLength))
    val tpadding = (obsConfig.tiles - tiles.size) * tileFeat
    for (_ <- 0 until tpadding) bb.putFloat(0.0f)

    // All enemies, including drones not visible to player
    ob.allEnemyDrones.take(nEnemy).foreach(serializeDrone(ob, _))
    val aepadding = (nEnemy - ob.allEnemyDrones.size) * enemyDroneFeat
    for (_ <- 0 until aepadding) bb.putFloat(0.0f)
  }

  def serializeDrone(obs: Observation, drone: DroneObservation): Unit = {
    bb.putFloat(drone.xPos)
    bb.putFloat(drone.yPos)
    bb.putFloat(math.cos(drone.orientation).toFloat)
    bb.putFloat(math.sin(drone.orientation).toFloat)
    bb.putFloat(drone.storedResources)
    bb.putFloat(if (drone.isConstructing) 1.0f else -1.0f)
    bb.putFloat(if (drone.isHarvesting) 1.0f else -1.0f)
    bb.putFloat(drone.hitpoints)
    bb.putFloat(drone.storageModules)
    bb.putFloat(drone.missileBatteries)
    bb.putFloat(drone.constructors)
    bb.putFloat(drone.engines)
    bb.putFloat(drone.shieldGenerators)
    bb.putFloat(if (drone.isStunned) 1.0f else -1.0f)
    bb.putFloat(if (drone.isEnemy) -1.0f else 1.0f)
    if (obsConfig.lastSeen) {
      bb.putFloat(drone.timeSinceVisible.toFloat)
      bb.putFloat(drone.missileCooldown.toFloat)
    }
    if (obsConfig.isVisible) {
      bb.putFloat(if (drone.isVisible) 1.0f else -1.0f)
    }
    if (obsConfig.obsLastAction && !drone.isEnemy) {
      val action = drone.lastAction.map(_.toInt).getOrElse(0)
      for (i <- 0 until 8) {
        if (i == action) {
          bb.putFloat(1)
        } else {
          bb.putFloat(0)
        }
      }
    }
    if (obsConfig.lockBuildAction) {
      bb.putFloat(if (drone.buildActionLocked) 1.0f else 0.0f)
    }
    if (obsConfig.distanceToWall) {
      bb.putFloat(distanceToWall(obs, drone.xPos, drone.yPos, drone.orientation - math.Pi / 2))
      bb.putFloat(distanceToWall(obs, drone.xPos, drone.yPos, drone.orientation - math.Pi / 4))
      bb.putFloat(distanceToWall(obs, drone.xPos, drone.yPos, drone.orientation))
      bb.putFloat(distanceToWall(obs, drone.xPos, drone.yPos, drone.orientation + math.Pi / 4))
      bb.putFloat(distanceToWall(obs, drone.xPos, drone.yPos, drone.orientation + math.Pi / 2))
    }
  }

  def serializeMineral(m: MineralObservation): Unit = {
    bb.putFloat(m.xPos)
    bb.putFloat(m.yPos)
    bb.putFloat(m.size)
    if (obsConfig.mineralAvailable) {
      bb.putFloat(if (m.claimed) 0.0f else 1.0f)
    }
  }

  def serializeTile(time: Int, tmax: Int)(t: MapTile): Unit = {
    bb.putFloat(t.centerX)
    bb.putFloat(t.centerY)
    bb.putFloat(if (t.lastVisitedTime == 0) tmax else time - t.lastVisitedTime)
    bb.putFloat(if (t.lastVisitedTime == 0) { -1 } else { 1 })
  }

  def serializeScores(ob: Observation): Unit = {
    bb.putFloat(ob.winner.map(_ + 1.0f).getOrElse(0))
    bb.putFloat(ob.alliedScore.toFloat)
    bb.putFloat(ob.enemyScore.toFloat)
    bb.putFloat(ob.minAlliedMSHealth.toFloat)
    bb.putFloat(ob.minEnemyMSHealth.toFloat)
  }

  def serializeActionMasks(ob: Observation): Unit = {
    for (i <- 0 until obsConfig.allies) {
      if (ob.alliedDrones.size <= i) {
        for (_ <- 0 until naction) bb.putFloat(0.0f)
      } else {
        val drone = ob.alliedDrones(i)
        // 0-5: turn/movement (4 is no turn, no movement)
        // 6: build [0,1,0,0,0] drone
        // 7: deposit resources (previously harvest)
        val canMove = if (drone.isStunned || drone.harvestLock) 0.0f else 1.0f
        for (i <- 0 until 6) {
          // Can always do nothing
          if (i == 4)
            bb.putFloat(1.0f)
          else
            bb.putFloat(canMove)
        }
        val canConstruct =
          if (drone.constructors > 0 &&
              !drone.isConstructing &&
              !(drone.buildActionLocked && obsConfig.lockBuildAction) &&
              drone.storedResources > 0 &&
              !(obsConfig.enforceUnitCap && ob.alliedDrones.size >= obsConfig.allies))
            1.0f
          else 0.0f
        bb.putFloat(canConstruct)
        bb.putFloat(if (drone.storedResources > 0) 1.0f else 0.0f)
        for (_ <- obsConfig.extraBuildActions) {
          bb.putFloat(canConstruct)
        }
        if (obsConfig.lockBuildAction) {
          if (drone.constructors == 0) {
            bb.putFloat(0.0f)
            bb.putFloat(0.0f)
          } else {
            bb.putFloat(if (drone.buildActionLocked) 0.0f else 1.0f)
            bb.putFloat(if (drone.buildActionLocked) 1.0f else 0.0f)
          }
        }
      }
    }
  }

  def distanceToWall(obs: Observation, x: Float, y: Float, orientation: Double): Float = {
    val o = Vector2(orientation)
    val pos = Vector2(x, y)

    var minDist = Double.MaxValue

    if (o.x != 0) {
      // Top wall
      val intersectYTop = y + o.y * (obs.mapHeight * 0.5 - x) / o.x
      val distTop = (Vector2(obs.mapHeight * 0.5, intersectYTop) - pos).dot(o)
      if (distTop > 0 && distTop < minDist) {
        minDist = distTop
      }

      // Bottom wall
      val intersectYBot = y + o.y * (-obs.mapHeight * 0.5 - x) / o.x
      val distBot = (Vector2(-obs.mapHeight * 0.5, intersectYBot) - pos).dot(o)
      if (distBot > 0 && distBot < minDist) {
        minDist = distBot
      }
    }

    if (o.y != 0) {
      // Right wall
      val intersectXRight = x + o.x * (obs.mapWidth * 0.5 - y) / o.y
      val distRight = (Vector2(intersectXRight, obs.mapWidth * 0.5) - pos).dot(o)
      if (distRight > 0 && distRight < minDist) {
        minDist = distRight
      }

      // Left wall
      val intersectXLeft = x + o.x * (-obs.mapWidth * 0.5 - y) / o.y
      val distBot = (Vector2(intersectXLeft, -obs.mapWidth * 0.5) - pos).dot(o)
      if (distBot > 0 && distBot < minDist) {
        minDist = distBot
      }
    }

    math.min(1000.0, minDist).toFloat
  }
}
