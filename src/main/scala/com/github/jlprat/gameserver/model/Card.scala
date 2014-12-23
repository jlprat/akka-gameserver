package com.github.jlprat.gameserver.model

object Card {
  def apply(id: Int, rank: Int, suit: String) = new Card(id, rank, suit)
}

/**
 * It models a Card. It contains an Id because it might have duplicates
 * Created by josep on 12/23/14.
 */
class Card (val id: Int, val rank: Int, val suit: String) {

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
    s"Card {id: $id, rank: $rank, suit: $suit}"
  }
}
