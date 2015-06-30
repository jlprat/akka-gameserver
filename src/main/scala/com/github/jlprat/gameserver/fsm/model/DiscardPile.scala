package com.github.jlprat.gameserver.fsm.model

/**
 * Companion object for the DiscardPile
 */
object DiscardPile {

  /**
   * creates a new Discard pile with the specific card
   * @param playedCard the card to add to the discard pile
   * @return the desired discard pile
   */
  def apply(playedCard: PlayedCard) = new DiscardPile(List(playedCard))

  def empty = new DiscardPile(Nil)
}

/**
 * This class represents the discarded cards from all players
 * Created by josep on 12/23/14.
 */
case class DiscardPile (cards: List[PlayedCard]) {

  /**
   * discards a new card to the discard pile
   * @param playedCard the card to be discarded
   * @return a new discarded pile with this card on top
   */
  def ::(playedCard: PlayedCard) = DiscardPile(playedCard :: cards)

  /**
   * @return the size of the discard pile
   */
  def size: Int = cards.size

  /**
   * @return the top card of the discard pile if there is any
   */
  def topCard: Option[PlayedCard] = cards.headOption

}
