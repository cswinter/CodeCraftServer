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

  def startGame(maxTicks: Option[Int], actionDelay: Int) = Action {
    val id = multiplayerServer.startGame(maxTicks)
    Ok(f"""{"id": $id}""").as("application/json")
  }

  def act(gameID: Int, playerID: Int) = Action { implicit request =>
    val action = read[Action](request.body.asJson.get.toString)
    multiplayerServer.act(gameID, playerID, action)
    Ok("success").as("application/json")
  }

  def playerState(gameID: Int, playerID: Int) = Action {
    val payload = multiplayerServer.observe(gameID, playerID)
    Ok(write(payload)).as("application/json")
  }

  def batchAct() = Action { implicit request =>
    val actions = read[Map[String, Action]](request.body.asJson.get.toString)
    for ((gameID, action) <- actions) {
      multiplayerServer.act(gameID.toInt, 0, action)
    }
    Ok("success").as("application/json")
  }

  def batchPlayerState(json: Boolean) = Action { implicit request =>
    val games = read[Seq[Int]](request.body.asJson.get.toString)
    val payload: Seq[Observation] = for (gameID <- games)
      yield multiplayerServer.observe(gameID, 0)
    if (json) {
      Ok(write(payload)).as("application/json")
    } else {
      Ok(serializeObs(payload)).as("application/octet-stream")
    }
  }

  def serializeObs(obs: Seq[Observation]): Array[Byte] = {
    val bb: ByteBuffer = ByteBuffer.allocate(4 * (obs.length * 49 + obs.length))
    bb.order(ByteOrder.nativeOrder)
    for (ob <- obs) {
      bb.putFloat(ob.timestep.toFloat / ob.maxGameLength)
      bb.putFloat(ob.alliedScore.toFloat)
      val drone = ob.alliedDrones(0)
      val x = drone.xPos
      val y = drone.yPos
      bb.putFloat(x / 1000.0f)
      bb.putFloat(y / 1000.0f)
      bb.putFloat(math.sin(drone.orientation).toFloat)
      bb.putFloat(math.cos(drone.orientation).toFloat)
      bb.putFloat(drone.storedResources / 50.0f)
      bb.putFloat(if (drone.isConstructing) 1.0f else -1.0f)
      bb.putFloat(if (drone.isHarvesting) 1.0f else -1.0f)
      for (m <- ob.minerals.sortBy(m => (m.xPos * m.xPos + m.yPos * m.yPos) / m.size).take(10)) {
        bb.putFloat((m.xPos - x) / 1000.0f)
        bb.putFloat((m.yPos - y) / 1000.0f)
        bb.putFloat(math.sqrt((m.yPos - y) * (m.yPos - y) + (m.xPos - x) * (m.xPos - x)).toFloat / 1000.0f)
        bb.putFloat(m.size / 100.0f)
      }
      for (m <- 0 until (10 - ob.minerals.size) * 4) {
        bb.putFloat(0.0f)
      }
    }
    for (ob <- obs) {
      bb.putFloat(ob.winner.map(_ + 1.0f).getOrElse(0))
    }
    bb.array()
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
