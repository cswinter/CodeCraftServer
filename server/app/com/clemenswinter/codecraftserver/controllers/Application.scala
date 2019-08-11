package com.clemenswinter.codecraftserver.controllers

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

  def startGame(maxTicks: Option[Int], actionDelay: Int = 0) = Action {
    val id = multiplayerServer.startGame(maxTicks, actionDelay)
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

  def batchPlayerState() = Action { implicit request =>
    val games = read[Seq[Int]](request.body.asJson.get.toString)
    val payload: Seq[Observation] = for (gameID <- games)
      yield multiplayerServer.observe(gameID, 0)
    Ok(write(payload)).as("application/json")
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
