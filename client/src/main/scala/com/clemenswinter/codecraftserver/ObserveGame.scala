package com.clemenswinter.codecraftserver

import cwinter.codecraft.core.api.{DroneControllerBase, TheGameMaster}
import cwinter.codecraft.core.game
import cwinter.codecraft.core.game.DroneWorldSimulator
import org.scalajs.dom
import org.scalajs.dom.{document, html}

import scala.scalajs.js.annotation.JSExport

object ObserveGame {
  def main(args: Array[String]): Unit = {
    val canvas: html.Canvas =
      document.getElementById("webgl-canvas").asInstanceOf[html.Canvas]
    val multiplayer = true

    new game.Settings(recordReplays = false).setAsDefault()
    TheGameMaster.canvas = canvas

    if (multiplayer) {
      val script = document.getElementById("script")
      val autorestart = script.getAttribute("data-autorestart") == "true"
      val autozoom = script.getAttribute("data-autozoom") == "true"
      runMultiplayer(autorestart, autozoom)
    } else {
      run(TheGameMaster.replicatorAI(), TheGameMaster.replicatorAI())
    }
  }

  def runMultiplayer(autorestart: Boolean, autozoom: Boolean): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    val simulator = TheGameMaster.prepareMultiplayerGame("localhost", TheGameMaster.replicatorAI())
    simulator.onSuccess {
      case s: DroneWorldSimulator =>
        if (autozoom) {
          val canvas: html.Canvas =
            document.getElementById("webgl-canvas").asInstanceOf[html.Canvas]
          val zoomX = math.log(s.config.worldSize.width * 1.05 / canvas.clientWidth)
          val zoomY = math.log(s.config.worldSize.height * 1.05 / canvas.clientHeight)
          s.initialCameraZoom = math.max(zoomX, zoomY).toFloat
        }
        TheGameMaster.run(s, onComplete = () => {
          if (autorestart) {
            scala.scalajs.js.timers.setTimeout(1000.0)(runMultiplayer(autorestart, autozoom))
          }
        })
    }
  }

  def run(m1: DroneControllerBase, m2: DroneControllerBase): Unit = {
    val simulator = new DroneWorldSimulator(
      TheGameMaster.defaultMap.createGameConfig(Seq(m1, m2), tickPeriod = 1))
    TheGameMaster.run(simulator, () => {})
  }
}
