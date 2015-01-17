package com.github.jlprat.gameserver.model

/**
 * Companion object for Deck
 * Created by josep on 12/23/14.
 */
object Deck {

  /**
   * Creates a new Deck object from a list of cards
   * @param deck the desired list of cards
   * @return the Deck containing the cards
   */
  def apply(deck: List[Card]) = new Deck(deck)

  /**
   * Creates an empty Deck, this is, with no cards
   * @return the empty Deck
   */
  def empty() = new Deck(List())
}

/**
 * Created by josep on 12/23/14.
 */
class Deck (val deck: List[Card]) {


  /**
   * Shuffles the deck
   * @return a shuffled deck
   */
  def shuffle: Deck = ???

  /**
   * Draws card to a specific number of players, taking cards one by one
   * @param numberPlayers the number of players to deal cards to
   * @param target the number of cards each player must have
   * @return an Option with as many Hand objects as players and the remaining Deck
   */
  def draw(numberPlayers: Int, target: Int): Option[(Seq[Hand], Deck)] = {
    draw(numberPlayers, target, 1)
  }

  /**
   * Draws card to a specific number of players, taking cards one by one
   * @param numberPlayers the number of players to deal cards to
   * @param target the number of cards each player must have
   * @param step the number of cards that should be drawn at a time for each player. Last step might be lower depending
   *             on how many cards should be still dealt
   * @return an Option with as many Hand objects as players and the remaining Deck
   */
  def draw(numberPlayers: Int, target:Int, step: Int): Option[(Seq[Hand], Deck)] = {
   take(numberPlayers * target).map(tuple => {
     val (drawnCards, remainingCards) = tuple
     val cards = drawnCards.cards.sliding(step, step).zipWithIndex

     ???
   })
  }

  /**
   * takes the 'n' top most card from the deck
   * @return The 'n' top most Card and the remaining Deck
   */
  def take(n:Int): Option[(Hand, Deck)] = {
    if(deck.size < n) None
    else {
      val (taken, rest) = deck.splitAt(n)
      Some(Hand(taken), Deck(rest))
    }
  }

  override def toString = {
    "Deck [" + deck.mkString(",") + "]"
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Deck]

  override def equals(other: Any): Boolean = other match {
    case that: Deck =>
      (that canEqual this) &&
        deck == that.deck
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(deck)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}
