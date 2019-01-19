package com.clemenswinter.codecraftserver.controllers

import javax.inject.{Inject, Singleton}

import cwinter.codecraft.core.multiplayer._
import akka.pattern.ask
import akka.util.Timeout
import play.api.inject.ApplicationLifecycle
import scala.concurrent.duration._
import scala.language.postfixOps

import scala.concurrent.Await
import scala.concurrent.Future

@Singleton
class MultiplayerServer @Inject()(lifecycle: ApplicationLifecycle) {
  println("Starting multiplayer server")
  val actorRef = Server.start(maxGames = 20)
  implicit val timeout = Timeout(10 seconds)
  val server = Await.result(actorRef ? Server.ScrewThis, 11 seconds).asInstanceOf[cwinter.codecraft.core.multiplayer.MultiplayerServer]

  new Thread {
    override def run {
      for (i <- 1 to 100) {
        server.startLocalGame()
        Thread.sleep(500)
      }
    }
  }.start()

  // it appears the class loader will not work during shutdown, so we need to get an instance of Server.Stop
  //       before the stop hook gets invoked
 val stop = Server.Stop
  lifecycle.addStopHook { () =>
    Future.successful(actorRef ! stop)
  }
}
