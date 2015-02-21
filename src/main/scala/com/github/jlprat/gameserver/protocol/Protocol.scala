package com.github.jlprat.gameserver.protocol

/**
 * All the protocol messages that are sent and received by the actors
 * Created by josep on 2/21/15.
 */
object Protocol {
  import com.github.jlprat.gameserver.model._

  case class PlayCard(card: Card, playerId: Int)
  case class PlayedCard(card: Card, playerId: Int)
  case class TakeCard(playerId: Int)
  case class TakenCard(card: Card, playerId: Int)
  case class TakenCardInfo(playerId: Int)
}
