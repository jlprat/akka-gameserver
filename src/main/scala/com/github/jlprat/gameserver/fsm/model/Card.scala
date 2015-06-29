package com.github.jlprat.gameserver.fsm.model

object RankOrdering extends Ordering[Card] {
  override def compare(x: Card, y: Card): Int = x.rank compare y.rank match {
    case 0 => x.suit compare y.suit
    case any => any
  }
}

object SuitOrdering extends Ordering[Card] {
  override def compare(x: Card, y: Card): Int = x.suit compare y.suit match {
    case 0 => x.rank compare y.rank
    case any => any
  }
}

object Card {
  case class CardId(id: Int) {
    def compare(other: CardId): Int = id compare other.id
  }
  case class Rank(rank: Int) {
    def compare(other: Rank): Int = rank compare other.rank
  }
  case class Suit(suit: String) {
    def compare(other: Suit): Int = suit compare other.suit
  }
}

import Card._
/**
 * It models a Card. It contains an Id because it might have duplicates
 * Created by josep on 12/23/14.
 */
case class Card (id: CardId, rank: Rank, suit: Suit) {

  def canEqual(other: Any): Boolean = other.isInstanceOf[Card]

  override def equals(other: Any): Boolean = other match {
    case that: Card =>
      (that canEqual this) &&
        id == that.id &&
        rank == that.rank &&
        suit == that.suit
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(id, rank, suit)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  override def toString(): String = {
    s"Card {id: ${id.id}, rank: ${rank.rank}, suit: ${suit.suit}}"
  }
}

case class PlayedCard(card: Card)