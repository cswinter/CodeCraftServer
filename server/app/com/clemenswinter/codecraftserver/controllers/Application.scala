package com.clemenswinter.codecraftserver.controllers

import javax.inject._
import betterviews._

import com.clemenswinter.codecraftserver.shared.SharedMessages
import play.api.mvc._

@Singleton
class Application @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  def index = Action {
    Ok(Index()).as("text/html")
  }

}
