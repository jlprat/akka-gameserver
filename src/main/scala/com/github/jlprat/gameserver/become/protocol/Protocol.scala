package com.github.jlprat.gameserver.become.protocol

/**
 * All the protocol messages that are sent and received by the actors
 * Created by josep on 2/21/15.
 */
object Protocol {
  import com.github.jlprat.gameserver.become.model.{Card,Hand}

  /**
   * Player tries to play a card
   * @param card to be played
   * @param playerId the player who is trying to play a card
   */
  case class PlayCard(card: Card, playerId: Int)

  /**
   * Notification to players that player {@code playerId} played a card regularly
   * @param card that has been played
   * @param playerId that made the move
   */
  case class PlayedCard(card: Card, playerId: Int)

  /**
   * Notification to players that player {@code playerId} played a card illegally (not valid)
   * @param card that has been played and rejected
   * @param playerId that made the move
   */
  case class PlayedCardIllegal(card: Card, playerId: Int)

  /**
   * Notification to players that inform which card is on the top of the discard pile
   * @param card
   */
  case class TopCard(card: Card)

  /**
   * Player asks for a new card
   * @param playerId the player who is trying to play a card
   */
  case class TakeCard(playerId: Int)

  /**
   * Player gets a new hand full of cards (1 to N)
   * @param hand the cards the player will get
   * @param playerId the player who will receive the cards
   */
  case class TakenCards(hand: Hand, playerId: Int)

  /**
   * Informative notification to be sent to players who are not {@code playerId}
   * @param numberCards the number of cards the player will get
   * @param playerId the player who will receive the cards
   */
  case class TakenCardInfo(numberCards: Int, playerId: Int)


  /**
   * Player asks to join the game
   */
  case object JoinGame

  /**
   * Successfully joined the game and player receives an ID
   * @param playerId the id for this player. From now on, this player will be identified with this code
   */
  case class JoinedGame(playerId: Int)

  /**
   * Informative notification to players that a new player joined the game
   * @param playerId the id of the new player
   */
  case class PlayerJoined(playerId: Int)


  /**
   * Player informs that leaves the game
   * @param playerId the player who is trying to play a card
   */
  case class Leave(playerId: Int)

  /**
   * Informative notification to players that a player left the game
   * @param playerId the id of the player who left
   */
  case class PlayerLeft(playerId: Int)


  /**
   * Player didn't act in given time
   * @param playerId the player who is trying to play a card
   */
  case class PlayerTimeOut(playerId: Int)

  /**
   * Informative notification to players that a player had a time out
   * @param playerId the id of the player who timed out
   */
  case class PlayerTimedOut(playerId: Int)


  /**
   * Notification to clients to know who is in turn
   * @param playerId the player who is in turn
   */
  case class NextTurn(playerId: Int)

  /**
   * Informative notification to players regarding a direction change 
   * @param clockwise if it is clockwise or not
   */
  case class ChangeDirection(clockwise: Boolean)

  /**
   * Informative notification to players regarding a player being skipped
   * @param playerId the id of the player being skipped
   */
  case class SkipPlayer(playerId: Int)

  /**
   * Informative notification to players regarding a player should play again
   * @param playerId the id of the player that must play again
   */
  case class PlayAgain(playerId: Int)


  /**
   * Player is asked to pick a color
   * @param playerId the player asked
   */
  case class ChangeSuitRequest(playerId: Int)

  /**
   * Player changes the color of the deck
   * @param suit the new suit of the top card
   * @param playerId the player who is trying to play a card
   */
  case class ChangeSuit(suit: String, playerId: Int)

  /**
   * The new suit of the top card is the one specified
   * @param suit the new suit of the top card
   */
  case class ChangedSuit(suit: String, playerId: Int)


  /**
   * The player announces Last Card
   * @param playerId the player who did the announcement
   */
  case class AnnounceLastCard(playerId: Int)

  /**
   * A player announced Last Card
   * @param playerId the player who announced
   */
  case class AnnouncedLastCard(playerId: Int)


  /**
   * The player finished the game (played all cards)
   */
  case object FinishedGame

  /**
   * Player is asked to show the cards
   */
  case object ShowCards

  /**
   * Player shows the current hand
   * @param hand the hand of the player
   * @param playerId the player who is showing the cards
   */
  case class CurrentHand(hand: Hand, playerId: Int)

  /**
   * Player gets the overall scores
   * @param results a map from playerId to Score
   */
  case class GameResults(results: Map[Int, Int])
}
