package com.clemenswinter.codecraftserver.controllers

import cwinter.codecraft.util.maths.{Rectangle, Vector2}
import cwinter.codecraft.core.multiplayer._
import cwinter.codecraft.core.api._
import cwinter.codecraft.core.game._

sealed trait Command
case class Move(direction: Double) extends Command
case class MoveTo(position: Vector2) extends Command
object Idle extends Command

class DemoController(
  commands: Seq[Command],
  delay: Int = 50
) extends DroneController {
  var i = 0

  override def onTick(): Unit = {
    if (missileCooldown == 0 && enemiesInSight.nonEmpty) {
      val closest = enemiesInSight.minBy(enemy => (enemy.position - position).lengthSquared)
      if (isInMissileRange(closest)) fireMissilesAt(closest)
    }

    Thread.sleep(1)

    if (i < delay) {
      halt()
    } else {
      commands.lift(i - delay) match {
        case Some(cmd) =>
          cmd match {
            case Idle =>
            case Move(d) => moveInDirection(d)
            case MoveTo(pos) => moveTo(pos)
          }
        case None =>
      }
    }

    i += 1

    if (storedResources >= 5 && constructors > 0 && !isConstructing) {
      if (constructors == 3) {
        buildDrone(new DemoController(Seq(MoveTo(Vector2(5000, -5000))), delay = 0),
                   missileBatteries = 1,
                   shieldGenerators = 1)
      } else {
        buildDrone(new AFK(), storageModules = 1, constructors = 1)
      }
    }

    if (constructors == 0 && storedResources > 0 && alliesInSight.nonEmpty) {
      for {
        ally <- alliesInSight
        if ally.storageModules > 0 && ally.constructors > 0 && (position - ally.position).length < 100
      } giveResourcesTo(ally)
    }

    if (mineralsInSight.nonEmpty && storedResources < storageModules * 7) {
      val closest = mineralsInSight.minBy(mc => (mc.position - position).lengthSquared)
      if (isInHarvestingRange(closest)) {
        harvest(closest)
      }
    }
  }
}

object Demo {
  def create(): (Seq[DroneController],
                 (Rectangle, Seq[Spawn], Either[Seq[(Int, Int)], List[(Vector2, Int)]], Boolean)) = {
    val initialDrones = None
    val mapWidth = 10000
    val mapHeight = 10000

    val drones1 = Seq(
      // DEMO 1: harvest (1s)
      (
        StartingDrone(-500, 1000, storageModules = 1),
        new DemoController(
          Seq(MoveTo(Vector2(-50, 1000))) ++
            Seq.fill(50)(Idle) ++
            Seq(MoveTo(Vector2(-500, 1000)))
        )
      ),
      // DEMO 2: give resources, produce (1s3c, 1s)
      (
        StartingDrone(-100, 3000, storageModules = 1, constructors = 3),
        new DemoController(Seq(MoveTo(Vector2(-50, 3000))))
      ),
      (
        StartingDrone(500, 3000, storageModules = 1, resources = 7),
        new DemoController(Seq.fill(30)(Idle) ++ Seq(MoveTo(Vector2(30, 3000))))
      ),
      // DEMO 3: missile (1m vs 1s)
      (
        StartingDrone(-500, 2000, missileBatteries = 1),
        new DemoController(Seq(MoveTo(Vector2(100, 2000))))
      ),
      // DEMO 4: shield  (1m1p vs 1m)
      (
        StartingDrone(-500, 0, missileBatteries = 1, shieldGenerators = 1),
        new DemoController(Seq(MoveTo(Vector2(0, 0))) ++ Seq.fill(100)(Idle) ++ Seq(MoveTo(Vector2(500, 0))))
      ),
      // DEMO 5: speed + collision (1m1p vs 3 x 1m)
      (
        StartingDrone(-800, -1000, missileBatteries = 1, shieldGenerators = 1),
        new DemoController(Seq(MoveTo(Vector2(100, -1000))))
      ),
      // DEMO 6: engine  (2e1p1e vs 2m)
      (
        StartingDrone(-800, -2000, missileBatteries = 2, shieldGenerators = 1, engines = 1),
        new DemoController(Seq(MoveTo(Vector2(1000, -2000))))
      ),
      // DEMO 7: large   (mothership vs ...)
      (
        StartingDrone(-400,
                      -3000,
                      missileBatteries = 3,
                      constructors = 3,
                      storageModules = 3,
                      shieldGenerators = 1),
        new DemoController(Seq(MoveTo(Vector2(0, -3000))))
      )
    )

    val drones2 = Seq(
      // DEMO 3: missile (1m vs 1s)
      (StartingDrone(0, 2500, storageModules = 1, resources = 7),
       new DemoController(Seq(MoveTo(Vector2(0, 2000))))),
      // DEMO 4: shield  (1m1p vs 1m)
      (
        StartingDrone(550, 50, missileBatteries = 1),
        new DemoController(Seq(MoveTo(Vector2(100, 50))))
      ),
      (
        StartingDrone(500, -50, missileBatteries = 1),
        new DemoController(Seq(MoveTo(Vector2(100, -50))))
      ),
      // DEMO 5: speed + collision (1m1p vs 3 x 1m)
      (
        StartingDrone(-1500, -1050, missileBatteries = 1),
        new DemoController(Seq(MoveTo(Vector2(100, -1050))))
      ),
      (
        StartingDrone(-1500, -950, missileBatteries = 1),
        new DemoController(Seq(MoveTo(Vector2(100, -950))))
      ),
      (
        StartingDrone(1500, -1000, missileBatteries = 1),
        new DemoController(Seq(MoveTo(Vector2(0, -1000))))
      ),
      // DEMO 6: engine  (2e1p1e vs 2m)
      (
        StartingDrone(-450, -2050, missileBatteries = 2),
        new DemoController(Seq(MoveTo(Vector2(300, -2050))))
      ),
      // DEMO 7: large   (mothership vs ...)
      (
        StartingDrone(500, -3300, shieldGenerators = 1, missileBatteries = 3),
        new DemoController(Seq(MoveTo(Vector2(0, -2980))))
      )
    )

    val map = (
      new Rectangle(-5000, 5000, -5000, 5000),
      drones1.map(_._1.toSpawn(BluePlayer)) ++ drones2.map(_._1.toSpawn(OrangePlayer)),
      Right(
        List(
          (Vector2(0, 1000), 100),
          (Vector2(0, 3000), 5),
          (Vector2(20, -3030), 200)
        )),
      false
    )

    val controllers = drones1.map(_._2) ++ drones2.map(_._2)

    (controllers, map)
  }
}
