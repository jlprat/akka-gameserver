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

  /**
   * Generates an empty hand
   * @return the empty Hand
   */
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
   * To get the number of cards that form this hand
   * @return the number of cards
   */
  def size = {
    cards.size
  }

  /**
   * Plays a card from the hand 
   * @param card the card that should be played
   * @return an optional Card (if present) and the remaining Hand
   */
  def play(card: Card): (Option[Card], Hand) = {
    (cards.find(_ == card), Hand(cards.filterNot(_ == card)))
  }

  /**
   * Checks if there is a card that satisfies <code>f</code>
   * @param f function to test the cards against
   * @return <code>true</code> if there is a card that satisfies <code>f</code>
   */
  def exists(f: Card => Boolean): Boolean = {
    cards.exists(f)
  }

  /**
   * Sorts the hand following the 'default' (by suit) criteria
   * @return a new Hand sorted
   */
  def sort: Hand = Hand(cards.sorted(SuitOrdering))

  /**
   * Sorts the hand following the 'default' (by suit) criteria
   * @return a new Hand sorted
   */
  def sortBySuit: Hand = sort

  /**
   * Sorts the hand following the rank criteria
   * @return a new Hand sorted by rank
   */
  def sortByRank: Hand = Hand(cards.sorted(RankOrdering))

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
