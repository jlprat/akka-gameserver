package com.github.jlprat.gameserver.model

/**
 * Companion object for Hand
 * Created by josep on 12/23/14.
 */
object Hand {

  /**
   * Generates a hand from a single card
   * @param card the card to generate a hand from
   * @return a Hand containing the desired card
   */
  def apply (card: Card) = new Hand(List(card))

  /**
   * Generates a hand from a collection of cards
   * @param cards the cards to generate a hand from
   * @return a Hand containing the desired cards
   */
  def apply (cards: List[Card]) = new Hand(cards)

  def apply () = new Hand(List())
}

/**
 * Hand is a representation of a bunch of Cards. Typically this will represent the cards the player has in his hands
 * @param cards
 */
class Hand (val cards: List[Card]) {

  /**
   * Merges to different Hands
   * @param other the other hand to merge to
   * @return a merged Hand containing all cards from both hands
   */
  def ::: (other: Hand) = {
    Hand(cards ::: other.cards)
  }
  
  /**
   * Sorts the hand following the 'default' criteria
   * @return a new Hand sorted
   */
  def sort: Deck = ???

  override def toString = {
    "Hand [" + cards.mkString(",") + "]"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Hand]

  override def equals(other: Any): Boolean = other match {
    case that: Hand =>
      (that canEqual this) &&
        cards == that.cards
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(cards)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
