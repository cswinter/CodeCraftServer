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

  def startGame = Action {
    val id = multiplayerServer.startGame()
    Ok(f"{id: $id}").as("application/json")
  }

  def act(gameID: Int, playerID: Int) = Action {
    multiplayerServer.act(gameID, playerID)
    Ok("success").as("application/json")
  }

  def playerState(gameID: Int, playerID: Int) = Action {
    val payload = multiplayerServer.observe(gameID, playerID)
    Ok(write(payload)).as("application/json")
  }

  def mpssJson = Action.async { implicit request =>
    val maxGameStats = request.getQueryString("maxgames").fold(250)(x => Try { x.toInt }.getOrElse(250))
    implicit val timeout = Timeout(1 seconds)
    val serverStatusFuture = multiplayerServer.actorRef ? Server.GetDetailedStatus
    for {
      untypedStatus <- serverStatusFuture
      status = untypedStatus.asInstanceOf[DetailedStatus]
      lessGames = status.games.sortBy(-_.startTimestamp).take(maxGameStats)
    } yield Ok(write(status.copy(games = lessGames))).as("application/json")
  }
}
