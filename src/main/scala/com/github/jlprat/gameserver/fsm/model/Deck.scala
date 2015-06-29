package com.github.jlprat.gameserver.fsm.model

import scala.util.Random

/**
 * Companion object for Deck
 * Created by josep on 12/23/14.
 */
object Deck {

  /**
   * Creates an empty Deck, this is, with no cards
   * @return the empty Deck
   */
  def empty = new Deck(List())
}

/**
 * Class that models a card deck
 * Created by josep on 12/23/14.
 */
case class Deck(deck: List[Card]) {


  /**
   * Shuffles the deck
   * @return a shuffled deck
   */
  def shuffle(seed: Long = 123456L): Deck = {
    val randomGenerator = new Random(seed)
    Deck(randomGenerator.shuffle(deck))
  }

  /**
   * @return the number of cards in the deck
   */
  def size = {
    deck.size
  }

  /**
   * Draws card to a specific number of players, taking cards one by one
   * @param numberPlayers the number of players to deal cards to
   * @param target the number of cards each player must have
   * @return an Option with as many Hand objects as players and the remaining Deck
   */
  def draw(numberPlayers: Int, target: Int): Option[(Iterable[Hand], Deck)] = {
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
  def draw(numberPlayers: Int, target: Int, step: Int): Option[(Iterable[Hand], Deck)] = {
    val (intermediate, last) = (target - (target % step), target % step)
    take(numberPlayers * intermediate).flatMap{ case (drawnCards, remainingCards) => {
      val grouped = drawnCards.cards.sliding(step, step).zipWithIndex.toList.groupBy(_._2 % numberPlayers)
      val hands = for {
        i <- 0 to (numberPlayers-1)
        cs = grouped(i).map(_._1).flatten
      } yield Hand(cs)
      if (last > 0) {
        remainingCards.draw(numberPlayers, last, last).map{case (lastHands, finalDeck) =>
          (lastHands.zip(hands).map{ case (lastHand, previousHand) => lastHand ::: previousHand}, finalDeck)
        }
      } else  Some(hands, remainingCards)
    }}
  }

  /**
   * takes the 'n' top most card from the deck
   * @return The 'n' top most Card and the remaining Deck
   */
  def take(n: Int): Option[(Hand, Deck)] = {
    if (deck.size < n) None
    else {
      val (taken, rest) = deck.splitAt(n)
      Some(Hand(taken), Deck(rest))
    }
  }

  /**
   * Takes cards until the predicate {@code p} is satisfied, all cards until finding the card that satisfies {@code p}
   * are also included in the result
   * @param p a predicate that must be run against the cards of the deck
   * @return None if there is no card in the deck that satisfies {@code p}. Some[Hand, Deck] with all the cards drawn
   *      from the deck until a card that satisfies {@code p} is drawn, and the remaining deck
   */
  def takeUntil(p: Card => Boolean): Option[(Hand, Deck)] = {
    val takenCards = deck.takeWhile(!p(_))
    take(takenCards.size + 1)
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
