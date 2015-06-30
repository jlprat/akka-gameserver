package com.github.jlprat.gameserver.fsm.model

import org.scalatest.WordSpec

/**
 * Defines the Specification of the Deck domain class
 * Created by josep on 1/31/15.
 */
class DeckSpec extends WordSpec{

  import Card._

  val numberCards = 52
  //TODO think about creating helpers for Deck Creation
  val deck = Deck(List.tabulate(numberCards)(elem => Card(CardId(elem), Rank(elem), Suit("suit"))))
  val cards = List.tabulate(5)(elem => Card(CardId(elem), Rank(elem), Suit("suit")))

  "An initial Deck" should {
    "have "+ numberCards +" cards" in {
      assert(deck.size === numberCards)
    }
    "have all cards with unique Id's" in {
      assert(deck.deck.map(_.id).distinct.size === numberCards)
    }
  }
  "A deck" can {
    "be created empty" in {
      assert(Deck.empty.size === 0)
    }
    "be created from a list of Cards" in {
      assert(Deck(cards).size === cards.size)
    }
  }

  "A deck" when {
    "not empty" should {
      "let me pick a card" which {
        val takenOption = deck.take(1)
        assert(takenOption.isDefined)
        val (bunch, restDeck) = takenOption.get
        assert(bunch.cards.size === 1)
        "is the top card of the deck" in {
          assert(bunch.cards.exists(_ == deck.deck.head))
        }
        "and the remaining deck has one card less" in {
          assert(restDeck.size === numberCards - 1)
        }
      }
      "prevent me from picking more cards than the deck itself" in {
        assert(deck.take(100).isEmpty)
      }
      "let me draw cards to players" when {
        "2 players are present" must {
          val drawnOption = deck.draw(2,6,1)
          assert(drawnOption.isDefined)
          val (bunches, restOfDeck) = drawnOption.get
          "have 2 bunches" in {
            assert(bunches.size === 2)
            assert(bunches.forall(_.cards.size === 6))
          }
          "have 1st player hand" which {
            "has exactly these cards" in {
              assert(bunches.head === Bunch(List.tabulate(6)(elem => Card(CardId(elem * 2), Rank(elem * 2), Suit("suit")))))
            }
          }
          "have 2nd player hand" which {
            "has exactly these cards" in {
              assert(bunches.last === Bunch(List.tabulate(6)(elem => Card(CardId(elem * 2 + 1), Rank(elem * 2 + 1), Suit("suit")))))
            }
          }
          "the remaining deck" must {
            "have 12 cards less" in {
              assert(restOfDeck.size === numberCards - 12)
            }
            "have the 13th card as a top card" in {
              assert(restOfDeck.take(1).map(_._1).get === Bunch(List(Card(CardId(12), Rank(12), Suit("suit")))))
            }
          }
        }
        "taking 2 cards at a time for 2 players" must {
          val drawnOption = deck.draw(3,6,2)
          assert(drawnOption.isDefined)
          val (bunches, restOfDeck) = drawnOption.get
          "have 3 bunches with 6 cards each" in {
            assert(bunches.size === 3)
            assert(bunches.forall(_.cards.size === 6))
          }
          "have the following cards for 1st player" in {
            assert(bunches.head === Bunch(List.tabulate(3)(elem => List(Card(CardId(elem * 6), Rank(elem * 6), Suit("suit")), Card(CardId(elem * 6 + 1), Rank(elem * 6 + 1), Suit("suit")))).flatten))
          }
          "have the following cards for 2nd player" in {
            assert(bunches.tail.head === Bunch(List.tabulate(3)(elem => List(Card(CardId(elem * 6 + 2), Rank(elem * 6 + 2), Suit("suit")), Card(CardId(elem * 6 + 3), Rank(elem * 6 + 3), Suit("suit")))).flatten))
          }
          "have the following cards for 3rd player" in {
            assert(bunches.last === Bunch(List.tabulate(3)(elem => List(Card(CardId(elem * 6 + 4), Rank(elem * 6 + 4), Suit("suit")), Card(CardId(elem * 6 + 5), Rank(elem * 6 + 5), Suit("suit")))).flatten))
          }
          "top Card on the deck is correct" in {
            assert(restOfDeck.deck.head === Card(CardId(18), Rank(18), Suit("suit")))
          }
        }
        "drawing odd number of cards taking by 2" must {
          val drawnOption = deck.draw(2,5,2)
          assert(drawnOption.isDefined)
          val (bunches, restOfDeck) = drawnOption.get
          "have 2 bunches with 5 cards each" in {
            assert(bunches.size === 2)
            assert(bunches.forall(_.cards.size === 5))
          }
          "have the following cards for 1st player" in {
            assert(bunches.head === Bunch(List(Card(CardId(0), Rank(0), Suit("suit")), Card(CardId(1), Rank(1), Suit("suit")), Card(CardId(4), Rank(4), Suit("suit")), Card(CardId(5), Rank(5), Suit("suit")), Card(CardId(8), Rank(8), Suit("suit")))))
          }
          "have the following cards for 2nd player" in {
            assert(bunches.last === Bunch(List(Card(CardId(2), Rank(2), Suit("suit")), Card(CardId(3), Rank(3), Suit("suit")), Card(CardId(6), Rank(6), Suit("suit")), Card(CardId(7), Rank(7), Suit("suit")), Card(CardId(9), Rank(9), Suit("suit")))))
          }
          "top Card on the deck is correct" in {
            assert(restOfDeck.deck.head === Card(CardId(10), Rank(10), Suit("suit")))
          }
        }
        "drawing odd number of cards taking by 3" must {
          val drawnOption = deck.draw(2,5,3)
          assert(drawnOption.isDefined)
          val (bunches, restOfDeck) = drawnOption.get
          "have 2 bunches with 5 cards each" in {
            assert(bunches.size === 2)
            assert(bunches.forall(_.cards.size === 5))
          }
          "have the following cards for 1st player" in {
            assert(bunches.head === Bunch(List(Card(CardId(0), Rank(0), Suit("suit")), Card(CardId(1), Rank(1), Suit("suit")), Card(CardId(2), Rank(2), Suit("suit")), Card(CardId(6), Rank(6), Suit("suit")), Card(CardId(7), Rank(7), Suit("suit")))))
          }
          "have the following cards for 2nd player" in {
            assert(bunches.last === Bunch(List(Card(CardId(3), Rank(3), Suit("suit")), Card(CardId(4), Rank(4), Suit("suit")), Card(CardId(5), Rank(5), Suit("suit")), Card(CardId(8), Rank(8), Suit("suit")), Card(CardId(9), Rank(9), Suit("suit")))))
          }
          "top Card on the deck is correct" in {
            assert(restOfDeck.deck.head === Card(CardId(10), Rank(10), Suit("suit")))
          }
        }
      }
      "prevent me from drawing more cards" when {
        "there are not enough cards in the deck" in {
          val drawnOption = deck.draw(10,6,2)
          assert(drawnOption.isEmpty)
        }
      }
    }
    "empty" should {
      "not let me pick a card" in {
        assert(Deck.empty.take(1).isEmpty)
      }
      "not let me draw cards to players" in {
        assert(Deck.empty.draw(2,4).isEmpty)
      }
    }
  }
  "A deck shuffle" must {
    val shuffledDeck = deck.shuffle()
    "change the order of the cards" in {
      assert(shuffledDeck !== deck)
    }
    "be consistent" in {
      assert(shuffledDeck === deck.shuffle())
    }
    "be different given different seeds" in {
      assert(shuffledDeck !== deck.shuffle(4348934L))
    }
    "not change the original deck" in {
      assert(deck === Deck(List.tabulate(numberCards)(elem => Card(CardId(elem), Rank(elem), Suit("suit")))))
    }
  }

  "A deck takeUntilFalse" must {
    "return None" when {
      "the predicate is not true for all cards in the Deck" in {
        val result = deck.takeUntil(_.suit == Suit("notThere"))
        assert(result.isEmpty)
      }
    }
    "return only 1 card" when {
      "the predicate is true for the first card drawn" in {
        val result = deck.takeUntil(_.id == CardId(0))
        assert(result.isDefined)
        assert(result.forall{
          case (bunch, remainingDeck) => bunch.cards.size == 1 && remainingDeck.size == numberCards - 1
        })
      }
    }
    "return more than 1 card" when {
      "the predicate is true for a card that is not the first one" in {
        val result = deck.takeUntil(_.id == CardId(4))
        assert(result.isDefined)
        assert(result.forall{
          case (bunch, remainingDeck) => bunch.cards.size == 5 && remainingDeck.size == numberCards - 5
        })
      }
    }
  }

}
