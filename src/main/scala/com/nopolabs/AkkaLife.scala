package com.nopolabs

import akka.actor._

/**
 * Any live cell with fewer than two live neighbours dies, as if caused by underpopulation.
 * Any live cell with more than three live neighbours dies, as if by overcrowding.
 * Any live cell with two or three live neighbours lives on to the next generation.
 * Any dead cell with exactly three live neighbours becomes a live cell.
 */

case class Spawn(x:Int,y:Int)
case class Survive(cell:ActorRef)
object Generate
object Count
object Report
case class Cells(cells:List[ActorRef])
case class LiveCell(x:Int,y:Int)
case class Neighbors(neighbors: Map[(Int,Int),Int])
case class Survivors(survivors: Set[(Int,Int)])

case class Cell(val x: Int, val y: Int) extends Actor {
  def receive = {
    case Report => {
      sender ! LiveCell(x,y)
    }
    case Survivors(survivors: Set[(Int,Int)]) => {
      if (survivors.contains((x,y))) {
        sender ! Survive(self)
      } else {
        context.stop(self)
      }
    }
  }
}

class Generation(size: Int) extends Actor {
  import context._

  var nextGen: List[ActorRef] = Nil
  var gen: List[ActorRef] = Nil
  var liveCells: List[(Int,Int)] = List[(Int,Int)]()
  var unreported = size
  var generation = 0

  def receive = generating

  def generating: Receive = {
    case Spawn(x:Int,y:Int) => {
      val cell = context.actorOf(Props(new Cell(x,y)))
      nextGen = cell :: nextGen
      unreported = unreported - 1
      if (unreported == 0) {
        startCounting()
      }
    }
    case Survive(cell:ActorRef) => {
      nextGen = cell :: nextGen
      unreported = unreported - 1
      if (unreported == 0) {
        startCounting()
      }
    }
  }

  def counting: Receive = {
    case LiveCell(x:Int,y:Int) => {
      liveCells = (x,y) :: liveCells
      unreported = unreported - 1
      if (unreported == 0) {
        unbecome()
        val neighbors = collectNeighbors(liveCells)
        val survivors = findSurvivors(neighbors, liveCells).toSet
        val spawn = findSpawn(neighbors, liveCells.toSet)
        unreported = survivors.size + spawn.size
        gen map { c => c ! Survivors(survivors) }
        spawn map { loc => self ! Spawn(loc._1,loc._2)}
      }
    }
  }

  def startCounting() {
    become(counting)
    generation = generation + 1
    println("Gen: " + generation)
    gen = nextGen
    nextGen = Nil
    liveCells = Nil
    unreported = gen.size
    gen map { c => c ! Report }
  }

  def isSurvivor(neighbors:Map[(Int,Int),Int], loc: (Int,Int)) = {
    val n = neighbors.getOrElse(loc, 0)
    (n == 2) || (n == 3)
  }

  def findSurvivors(neighbors:Map[(Int,Int),Int], liveCells: List[(Int,Int)]) = {
    for (loc <- liveCells if isSurvivor(neighbors, loc)) yield loc
  }

  def canSpawn(neighbors:Map[(Int,Int),Int], liveCells: Set[(Int,Int)], loc: (Int,Int)) = {
    (neighbors.getOrElse(loc, 0) == 3) && (!liveCells.contains(loc))
  }

  def findSpawn(neighbors:Map[(Int,Int),Int], liveCells: Set[(Int,Int)]) = {
    for (loc <- neighbors.keys if canSpawn(neighbors, liveCells, loc)) yield loc
  }

  def addNeighbors(neighbors:Map[(Int,Int),Int], loc:(Int,Int)) = loc match {
    case (x,y) => {
      var newNeighbors = neighbors
      for (nx <- (x-1) to (x+1);
           ny <- (y-1) to (y+1)
           if ((nx != x) || (ny != y))) {
        val l = (nx,ny)
        newNeighbors = newNeighbors + (l -> (neighbors.getOrElse(l, 0) + 1))
      }
      newNeighbors
    }
  }

  def collectNeighbors(liveCells: List[(Int,Int)]) = {
    liveCells.foldLeft(Map[(Int,Int),Int]())(addNeighbors)
  }
}

object AkkaLife extends App {
  val system = ActorSystem("AkkaLife")

  val startCells:List[(Int,Int)] = List((0,0),(0,1),(0,2),(1,0),(2,1))

  val gen = system.actorOf(Props(new Generation(startCells.size)))

  startCells.map(loc => gen ! Spawn(loc._1,loc._2))

  Thread.sleep(5000)

  system.shutdown()
}
