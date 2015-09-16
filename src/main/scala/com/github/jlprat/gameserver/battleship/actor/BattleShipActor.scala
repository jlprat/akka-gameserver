package com.github.jlprat.gameserver.battleship.actor

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
    case Event(PlaceShip(playerId, shipId, x, y, size, vertical), data: BattleshipData) =>
      goto(PlacingShips) using data.placeShip(playerId, shipId, x, y, size, vertical)
  }

  when(PlacingShips) {
    case Event(PlaceShip(playerId, shipId, x, y, size, vertical), data: BattleshipData) =>
      stay using data.placeShip(playerId, shipId, x, y, size, vertical)
    case Event(ShipsPlaced, data: BattleshipData) =>
      goto(WaitingForNextPlayer)
  }

  when(WaitingForNextPlayer) {
    case Event(NextPlayer, data: BattleshipData) =>
      stay using data.copy(currentPlayer = if (data.currentPlayer == 0) 1 else 0)
    case Event(PlaceShot(playerId, x, y), data: BattleshipData) =>
      goto(CheckingShot) using data.copy(pendingShot = Some(x, y))
  }

  when(CheckingShot) {
    case Event(Miss, data: BattleshipData) =>
      goto(WaitingForNextPlayer) using data.copy(pendingShot = None)
    case Event(Hit, data: BattleshipData) =>
      val newData = data.shoot._2
      goto(ShipHit) using newData.copy(pendingShot = None)
  }

  when(ShipHit) {
    case Event(ShipsAlive, data) =>
      goto(WaitingForNextPlayer) using data
    case Event(AllShipsSunk, _) =>
      goto(EndGame)
  }

  onTransition {
    case _ -> WaitingForNextPlayer =>
      self ! NextPlayer
    case _ -> PlacingShips =>
      val first = nextStateData.shipsToPlace(0)
      val second = nextStateData.shipsToPlace(1)
      if (List(first, second).forall(_.isEmpty)) self ! ShipsPlaced
    case _ -> CheckingShot =>
      if (nextStateData.wouldBeAShot)
        self ! Hit
      else
        self ! Miss
    case _ -> ShipHit =>
      if (nextStateData.areShipsAlive(nextStateData.opponent))
        self ! ShipsAlive
      else
        self ! AllShipsSunk
  }

  initialize()
}
