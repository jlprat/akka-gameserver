package com.github.jlprat.gameserver.become.model

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

/**
 * It models a Card. It contains an Id because it might have duplicates
 * Created by josep on 12/23/14.
 */
case class Card (id: Int, rank: Int, suit: String) {

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
