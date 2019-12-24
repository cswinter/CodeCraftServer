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
import play.api.mvc._

import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import upickle.default._

case class ObsConfig(allies: Int, drones: Int, minerals: Int, globalDrones: Int, relativePositions: Boolean)

@Singleton
class Application @Inject()(
  cc: ControllerComponents,
  val multiplayerServer: MultiplayerServer
) extends AbstractController(cc) {

  def index = Action {
    Ok(Index()).as("text/html")
  }

  def observe = Action {
    Ok(Observe()).as("text/html")
  }

  def startGame(maxTicks: Option[Int], actionDelay: Int, scriptedOpponent: Boolean) = Action {
    implicit request =>
      val body = request.body.asJson.get.toString
      val customMap = if (body == "\"\"") None else Some(read[MapSettings](body))
      val id = multiplayerServer.startGame(maxTicks, scriptedOpponent, customMap)
      Ok(f"""{"id": $id}""").as("application/json")
  }

  def act(gameID: Int, playerID: Int) = Action { implicit request =>
    val action = read[Action](request.body.asJson.get.toString)
    multiplayerServer.act(gameID, playerID, Seq(action))
    Ok("success").as("application/json")
  }

  def playerState(gameID: Int, playerID: Int) = Action {
    val payload = multiplayerServer.observe(gameID, playerID)
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

  def batchPlayerState(json: Boolean, allies: Int, drones: Int, minerals: Int, globalDrones: Int, relativePositions: Boolean) = Action {
    implicit request =>
      val obsConfig = ObsConfig(allies, drones, minerals, globalDrones, relativePositions)
      val games = read[Seq[(Int, Int)]](request.body.asJson.get.toString)
      val payload: Seq[Observation] = for ((gameID, playerID) <- games)
        yield multiplayerServer.observe(gameID, playerID)
      if (json) {
        Ok(write(payload)).as("application/json")
      } else {
        Ok(serializeObs(payload, obsConfig)).as("application/octet-stream")
      }
  }

  def serializeObs(obs: Seq[Observation], obsConfig: ObsConfig): Array[Byte] = {
    val droneProperties = 15
    val mineralProperties = 4
    val globals = 1
    val extras = 3
    val agentObs = globals + droneProperties +
      mineralProperties * obsConfig.minerals +
      droneProperties * obsConfig.drones
    val actionMasks = obsConfig.allies * 8
    val privilegedObs = obsConfig.globalDrones * droneProperties
    val nums = obs.length * (obsConfig.allies * agentObs + extras + actionMasks + privilegedObs)
    val bb: ByteBuffer = ByteBuffer.allocate(4 * nums)
    bb.order(ByteOrder.nativeOrder)
    for (ob <- obs) {
      val allDrones = ob.visibleEnemyDrones.map((_, true)) ++ ob.alliedDrones.map((_, false))

      // Controllable drones
      for (i <- 0 until obsConfig.allies) {
        if (ob.alliedDrones.size <= i) {
          for (_ <- 0 until droneProperties + globals) bb.putFloat(0.0f)
        } else {
          val drone = ob.alliedDrones(i)
          // Time
          bb.putFloat(ob.timestep.toFloat / ob.maxGameLength)
          // Allied drone
          val x = drone.xPos
          val y = drone.yPos
          bb.putFloat(x / 1000.0f)
          bb.putFloat(y / 1000.0f)
          bb.putFloat(math.sin(drone.orientation).toFloat)
          bb.putFloat(math.cos(drone.orientation).toFloat)
          bb.putFloat(drone.storedResources / 50.0f)
          bb.putFloat(if (drone.isConstructing) 1.0f else -1.0f)
          bb.putFloat(if (drone.isHarvesting) 1.0f else -1.0f)
          bb.putFloat(0.1f * drone.hitpoints)
          bb.putFloat(0.5f * drone.storageModules)
          bb.putFloat(0.5f * drone.missileBatteries)
          bb.putFloat(0.5f * drone.constructors)
          bb.putFloat(0.5f * drone.engines)
          bb.putFloat(0.5f * drone.shieldGenerators)
          bb.putFloat(if (drone.isStunned) 1.0f else -1.0f)
          bb.putFloat(1.0f)
        }
      }

      // Minerals
      for (i <- 0 until obsConfig.allies) {
        if (ob.alliedDrones.size <= i) {
          for (_ <- 0 until obsConfig.minerals * mineralProperties) bb.putFloat(0.0f)
        } else {
          val drone = ob.alliedDrones(i)
          val x_offset = if (obsConfig.relativePositions) drone.xPos else 0.0f
          val y_offset = if (obsConfig.relativePositions) drone.yPos else 0.0f
          for (m <- ob.minerals
                 .sortBy(m => (m.xPos * m.xPos + m.yPos * m.yPos) / m.size)
                 .take(obsConfig.minerals)) {
            bb.putFloat((m.xPos - x_offset) / 1000.0f)
            bb.putFloat((m.yPos - y_offset) / 1000.0f)
            bb.putFloat(
              math.sqrt((m.yPos - y_offset) * (m.yPos - y_offset) + (m.xPos - x_offset) * (m.xPos - x_offset)).toFloat / 1000.0f)
            bb.putFloat(m.size / 100.0f)
          }
          for (_ <- 0 until (obsConfig.minerals - ob.minerals.size) * mineralProperties) {
            bb.putFloat(0.0f)
          }
        }
      }

      // All drones
      for (i <- 0 until obsConfig.allies) {
        if (ob.alliedDrones.size <= i) {
          for (_ <- 0 until obsConfig.drones * droneProperties) bb.putFloat(0.0f)
        } else {
          val drone = ob.alliedDrones(i)
          val x_offset = if (obsConfig.relativePositions) drone.xPos else 0.0f
          val y_offset = if (obsConfig.relativePositions) drone.yPos else 0.0f
          val dronesSorted =
            allDrones.sortBy(d => (d._1.xPos - x_offset) * (d._1.xPos - x_offset) + (d._1.yPos - y_offset) * (d._1.yPos - y_offset))
          for ((drone, isEnemy) <- dronesSorted.slice(1, obsConfig.drones + 1)) {
            bb.putFloat((drone.xPos - x_offset) / 1000.0f)
            bb.putFloat((drone.yPos - y_offset) / 1000.0f)
            bb.putFloat(math.sin(drone.orientation).toFloat)
            bb.putFloat(math.cos(drone.orientation).toFloat)
            bb.putFloat(drone.storedResources / 50.0f)
            bb.putFloat(if (drone.isConstructing) 1.0f else -1.0f)
            bb.putFloat(if (drone.isHarvesting) 1.0f else -1.0f)
            bb.putFloat(0.1f * drone.hitpoints)
            bb.putFloat(0.5f * drone.storageModules)
            bb.putFloat(0.5f * drone.missileBatteries)
            bb.putFloat(0.5f * drone.constructors)
            bb.putFloat(0.5f * drone.engines)
            bb.putFloat(0.5f * drone.shieldGenerators)
            bb.putFloat(if (drone.isStunned) 1.0f else -1.0f)
            bb.putFloat(if (isEnemy) -1.0f else 1.0f)
          }
          for (_ <- 0 until (obsConfig.drones + 1 - allDrones.size) * droneProperties) {
            bb.putFloat(0.0f)
          }
        }
      }
    }

    // Scores, winner
    for (ob <- obs) {
      bb.putFloat(ob.winner.map(_ + 1.0f).getOrElse(0))
      bb.putFloat(ob.alliedScore.toFloat)
      bb.putFloat(ob.enemyScore.toFloat)
    }

    // Action masks
    for (ob <- obs) {
      for (i <- 0 until obsConfig.allies) {
        if (ob.alliedDrones.size <= i) {
          for (_ <- 0 until 8) bb.putFloat(0.0f)
        } else {
          val drone = ob.alliedDrones(i)
          // 0-5: turn/movement (4 is no turn, no movement)
          // 6: build [0,1,0,0,0] drone (if minerals > 5)
          // 7: harvest
          val canMove = if (drone.isStunned || drone.isConstructing) 0.0f else 1.0f
          for (_ <- 0 until 6) bb.putFloat(canMove)
          val canConstruct =
            if (drone.constructors > 0 && drone.storedResources >= 5 && !drone.isConstructing) 1.0f
            else 0.0f
          bb.putFloat(canConstruct)
          // TODO: harvest action
          bb.putFloat(0.0f)
        }
      }
    }

    // Privileged information
    for (ob <- obs) {
      val allDrones = ob.allEnemyDrones.map((_, -1.0f)) ++ ob.alliedDrones.map((_, 1.0f))
      for ((drone, isEnemy) <- allDrones.slice(0, obsConfig.globalDrones)) {
        bb.putFloat(drone.xPos / 1000.0f)
        bb.putFloat(drone.yPos / 1000.0f)
        bb.putFloat(math.sin(drone.orientation).toFloat)
        bb.putFloat(math.cos(drone.orientation).toFloat)
        bb.putFloat(drone.storedResources / 50.0f)
        bb.putFloat(if (drone.isConstructing) 1.0f else -1.0f)
        bb.putFloat(if (drone.isHarvesting) 1.0f else -1.0f)
        bb.putFloat(0.1f * drone.hitpoints)
        bb.putFloat(0.5f * drone.storageModules)
        bb.putFloat(0.5f * drone.missileBatteries)
        bb.putFloat(0.5f * drone.constructors)
        bb.putFloat(0.5f * drone.engines)
        bb.putFloat(0.5f * drone.shieldGenerators)
        bb.putFloat(if (drone.isStunned) 1.0f else -1.0f)
        bb.putFloat(isEnemy)
      }
      for (_ <- 0 until (obsConfig.globalDrones - allDrones.size) * droneProperties) {
        bb.putFloat(0.0f)
      }
    }

    assert(bb.position() == 4 * nums, f"Expected ${4 * nums} elements, actual: ${bb.position()}")
    val result = bb.array()
    result
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
    Ok(write(multiplayerServer.debugState)).as("application/json")
  }
}
