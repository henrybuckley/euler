package org.buckleys

object Card {
  val rankString = "23456789TJQKA"
}

class Card(name: String) extends Ordered[Card] {
  import Card._
  val suit: Char = name.charAt(1)
  val rank: Int = rankString.indexWhere(_ == name.charAt(0))
  def compare(that: Card): Int = rank.compareTo(that.rank)
  override def toString = rankString(rank).toString + suit
}

class Hand(cardNames: Array[String]) extends Ordered[Hand] {

  def this(s: String) = this(s.split("""\s"""))
  val cards: List[Card] = cardNames.map(new Card(_)).toList.sorted

  val handRank: Int = {
    val lastCardRankOption = Some(cards.last.rank)

    val isStraight = (cards.sliding(2).forall({ case List(a, b) => b.rank - a.rank == 1 }), lastCardRankOption)
    val isFlush = (cards.sliding(2).forall({ case List(a, b) => a.suit == b.suit }), lastCardRankOption)
    val isStraightFlush = (isFlush._1 && isStraight._1, lastCardRankOption)
    val isRoyalFlush = (isStraightFlush._1 && cards(0).rank == 10, lastCardRankOption)
    val groups = cards.groupBy(_.rank).values.toList
    val groupSizes = groups.map(_.size)
    val isFour = (groupSizes.contains(4), groups.find(_.size == 4).map(_.head.rank))
    val isThree = (groupSizes.contains(3), groups.find(_.size == 3).map(_.head.rank))
    val isPair = (groupSizes.contains(2), groups.find(_.size == 2).map(_.head.rank))
    val isFullHouse = (isThree._1 && isPair._1, isThree._2.flatMap(x => isPair._2.map(y => x * 15 + y)))
    val isTwoPair = (groupSizes.groupBy(identity).get(2).exists(_.size == 2), Some(groups.reverse.foldLeft(1)((prod, g) => { if (g.size == 2) prod * 15 + g.head.rank else prod })))
    val isHighCard = (true, lastCardRankOption)
    val flags = List(isRoyalFlush, isStraightFlush, isFour, isFullHouse, isFlush, isStraight, isThree, isTwoPair, isPair, isHighCard)
    val rankIndex = flags.indexWhere(_._1)
    val rank = (flags.length - rankIndex - 1) * 1000 + flags(rankIndex)._2.head
    rank
  }

  def handType = List("High Card", "Pair", "Two Pair", "Three of a kind", "Straight", "Flush", "Full House", "Four of a kind", "Straight Flush", "Royal Flush")(handRank / 1000)

  val cardRank: Int = cards.reverse.foldLeft(1)(_ * 15 + _.rank)

  def compare(that: Hand): Int = {
    val handRankCompare = handRank.compareTo(that.handRank)
    if (handRankCompare == 0) cardRank.compareTo(that.cardRank) else handRankCompare
  }
  override def toString = cards.mkString(" ") + ":" + handType
}