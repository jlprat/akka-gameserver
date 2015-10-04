package com.github.jlprat.gameserver.battleship.actor

import scala.concurrent.duration._
import akka.actor.{ActorLogging, FSM}
import com.github.jlprat.gameserver.battleship.model._

/**
 * Created by josep on 9/16/15.
 */
class BattleShipActor extends ActorLogging with FSM[BattleshipState, BattleshipData]{
  def getShips(): Array[Vector[Short]] = {
    val ships = Vector(5, 4, 4, 3, 3, 3, 2, 2, 2, 1, 1).map(_.toShort)
    List(ships, ships).toArray
  }

  def getData(): Array[Map[(Short, Short), CellStatus]] = {
    val empty: Map[(Short, Short), CellStatus] = Map.empty
    List(empty, empty).toArray
  }

  startWith(WaitingForPlayers, new BattleshipData(getData(), currentPlayer = 1, pendingShot = None, getShips()))

  when(WaitingForPlayers) {
    case Event(PlaceShip(playerId, shipId, x, y, size, vertical), data) =>
      if (data.canShipBePlaced(playerId, shipId, x, y, size, vertical))
        goto(PlacingShips) using data.placeShip(playerId, shipId, x, y, size, vertical)
      else stay
  }

  when(PlacingShips) {
    case Event(PlacedShip, _) =>
      goto(CheckingPlacedShips)
  }

  when(CheckingPlacedShips) {
    case Event(PlaceShip(playerId, shipId, x, y, size, vertical), data) =>
      goto(PlacingShips) using data.placeShip(playerId, shipId, x, y, size, vertical)
    case Event(ShipsPlaced, _) =>
      goto(WaitingForNextPlayer)
  }


  when(WaitingForNextPlayer, stateTimeout = 30 seconds) {
    case Event(NextPlayer, data) =>
      stay using data.copy(currentPlayer = data.opponent)
    case Event(PlaceShot(playerId, x, y), data) =>
      goto(CheckingShot) using data.copy(pendingShot = Some(x, y))
    case Event(StateTimeout, data) =>
      stay using data.copy(currentPlayer = data.opponent)
  }

  when(CheckingShot) {
    case Event(Miss, data) =>
      goto(WaitingForNextPlayer) using data.copy(pendingShot = None)
    case Event(Hit, data) =>
      val newData = data.shoot._2
      goto(HitShip) using newData.copy(pendingShot = None)
  }

  when(HitShip) {
    case Event(ShipsAlive, data) =>
      goto(WaitingForNextPlayer) using data
    case Event(AllShipsSunk, _) =>
      goto(EndGame)
  }

  onTransition {
    case _ -> PlacingShips => self ! PlacedShip
    case _ -> CheckingPlacedShips =>
      val first = nextStateData.shipsToPlace(0)
      val second = nextStateData.shipsToPlace(1)
      if (List(first, second).forall(_.isEmpty)) self ! ShipsPlaced
    case _ -> WaitingForNextPlayer =>
      log.info(s"End of ${nextStateData.currentPlayer} turn")
      self ! NextPlayer
    case _ -> CheckingShot =>
      if (nextStateData.wouldBeAShot) {
        log.info(s"Ship Hit at ${nextStateData.pendingShot}!")
        self ! Hit
      }
      else {
        log.info(s"Missed at ${nextStateData.pendingShot}!")
        self ! Miss
      }
    case _ -> HitShip =>
      if (nextStateData.areShipsAlive(nextStateData.opponent))
        self ! ShipsAlive
      else {
        self ! AllShipsSunk
      }
    case _ -> EndGame =>
      log.info("Game is Over")
  }

  initialize()
}
