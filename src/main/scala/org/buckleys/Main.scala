package org.buckleys

import util.control.Breaks._
import math._
import collection.mutable.Buffer
import collection.mutable.HashMap
import annotation.tailrec
import io._
import MathUtil._
import java.math.MathContext
import Factor._
import CollUtil._
import TestUtil._
import java.util.Date
object Main extends App {

  def problem98() = {
    val QuotedWord = """"(\w+)"""".r
    val words = Source.fromFile("resources/words.txt").mkString.split(",").
      map({ case QuotedWord(w) => w }).toList

    val squares = Stream.from(1).map(x => x * x).takeWhile(_ <= 1000000000).toList

    def sameAs[A](c: Traversable[A], d: Traversable[A]): Boolean =
      c.size == d.size && (c.toSeq diff d.toSeq).isEmpty

    def anagrams(word: String, wordList: List[String]) = wordList.filter(sameAs(_, word)) - word

    def tryMap[A, B](a: Seq[A], b: Seq[B]): Option[Map[A, B]] = {
      if (a.size != b.size) None
      else {
        val (adist, bdist, mdist) = (a.distinct, b.distinct, (a zip b).distinct)
        if (adist.size == bdist.size && mdist.size == adist.size) Some(mdist.toMap)
        else None
      }
    }

    def mapWord(w: String, mapping: Map[Char, Int]): Int = w.map(mapping).mkString.toInt

    def findMatches(w: String, a: String) =
      squares.flatMap(sq => {
        val mapping = tryMap(w.toList, sq.toString.map(_.asDigit))
        mapping.filter(m => m(a.head) != 0 && squares.contains(mapWord(a, m)))
      })

    val res = for {
      w <- words
      a <- anagrams(w, words.dropWhile(_ != w))
      m <- findMatches(w, a)
      s <- List(w, a).map(mapWord(_, m))
    } yield s

    println(res.max)
  }
  
  def problem97() = {
    //println(BigInt("1267650600228229401496703205376").pow(78304)  * BigInt(2).pow(57) * 28433)
    var n = BigInt(2)
    val modn = BigInt("100000000000")

    for (i <- 1 until 7830457) {
      n = (n * 2).mod(modn)
    }

    println((n * 28433 + 1).toString.takeRight(10))

    //println(n)

    //time(problem96())
  }

  def problem96() = {
    case class Sudoku(val grid: List[List[Int]]) {
      /**
       * Precalculate the non-zero values in each 3x3 subgrid.
       */
      val subgridVals = for (k <- 0 to 2; l <- 0 to 2)
        yield grid.slice(k * 3, k * 3 + 3).flatMap(_.slice(l * 3, l * 3 + 3)).toSet - 0

      /**
       * Non-zero row values for each row.
       */
      val rowVals = grid.map(_.toSet - 0)

      /**
       * Non-zero column values for each column.
       */
      val colVals = grid.transpose.map(_.toSet - 0)

      /**
       * Non-zero 3x3 subgrid values for the l'th subgrid in the k'th row (zero-indexed).
       */
      def subgridValsAt(k: Int, l: Int) = subgridVals((k / 3) * 3 + l / 3)

      /**
       * Return copy of this Sudoku board with an updated value at the specified position.
       */
      def updated(i: Int, j: Int, v: Int): Sudoku =
        Sudoku(grid.updated(i, grid(i).updated(j, v)))

      /**
       * Find grid coords of first empty cell, if any.
       */
      def findEmpty(): Option[(Int, Int)] = {
        val i = grid.indexWhere(row => row.contains(0))
        if (i >= 0) Some(i, (grid(i).indexWhere(_ == 0))) else None
      }

      /**
       * Return all numbers that are (potentially) legal for the given grid position.
       */
      def legalNumbersAt(i: Int, j: Int): Set[Int] =
        Set(1 to 9: _*) diff ((rowVals(i) union colVals(j) union subgridValsAt(i, j)) - grid(i)(j))

      def solve(): Sudoku = {
        def iter(board: Sudoku): Option[Sudoku] = {
          board.findEmpty match {
            case None => Some(board)
            case Some((i, j)) => {
              val solns = for (trialNumber <- board.legalNumbersAt(i, j).toStream)
                yield iter(board.updated(i, j, trialNumber))
              solns.find(!_.isEmpty).flatMap(x => x)
            }
          }
        }

        /*
    case None => Some(board)
        case Some((i, j)) => {
          val replaceNumbers = board.legalNumbersAt(i, j)
          if (replaceNumbers.isEmpty) None
          else {
            var s = Option.empty[Sudoku]
            val it = replaceNumbers.iterator
            while (s.isEmpty && it.hasNext) {
              s = iter(board.updated(i, j, it.next))
            }
            s
          }
        }

      }
*/
        iter(this).head
      }

      override def toString(): String = {
        val sb: StringBuilder = new StringBuilder()

        grid.grouped(3).foldLeft(sb)((sb, rowgrp) => {
          rowgrp.foldLeft(sb)((sb, row) => {
            row.grouped(3).foldLeft(sb)((sb, triplet) => {
              triplet.addString(sb, " ")
              sb.append("  ")
            })
            sb.append("\n")
          })
          sb.append("\n")
        })
        sb.toString
      }

    }

    val grids = Source.fromFile("resources/sudoku.txt").
      mkString.split("\n").
      grouped(10).
      map(_.toList.
        tail.
        map(_.toList.
          map(_.asDigit))).
      map(Sudoku(_)).toList

    //  println(grids(0))
    // println(grids(0).solve)
    println(grids.map(_.solve()).map(_.grid(0).take(3).mkString.toInt).sum)

    //println(grids(0).updated(1,3,13).updated(3,1,31).updated(2,4,24))

    //val g = grids(0).updated(0,0,1).updated(0,1,1).updated(0,3,1).updated(0,5,1).updated(0,7,1).updated(0,8,1)
    //println(g, g.findZero)
    //println(grids(0))

    //println(grids(0).canReplaceWith(0, 1))

    //grids.foreach(println)
  }

  /*
  def problem95() = {
    val nullentry = (-1, -1L)
    val memo = collection.mutable.Map[Long, (Int, Long)]()

    val ceiling = 1000000
    
    
    def amicableLength(n: Long): (Int, Long) = {
      /*
      def iter(i: Long, acc: Int): (Int, Long) = {
        val next = properDivisors(i).sum

        if (memo.contains(next)) memo(next)
        else if (next > ceiling || next <= 1) (-1, -1)
        else if (next == n) (acc, next)
        else iter(next, acc + 1)
      }
      */

      def findCycleLen(l: List[Long]): Int = {
        var i = -1;
        l.find(x => { i += 1; l.indexOf(x) < i })
        i
      }
    
      def iter(ceiling: Long): (Int, Long) = {
        var next = n
        var list = List[Long]()
        while (!list.contains(next)) {
          list = next :: list
          next = sumpropdiv(next)
          
          if (memo.contains(next)) {
            val res = memo(next)
            list.foreach(memo(_) = res) 
            return res
          }
          
          if (next > ceiling || next <= 1) {
            list.foreach(memo(_) = nullentry)
            return nullentry
          }
        }

        val cycleLength = findCycleLen(next :: list)
        list.foreach(memo(_) = (cycleLength, next))
        (cycleLength, next)
      }

      var res = memo.getOrElseUpdate(n, iter(ceiling))
      res
    }

    def alstream(n: Long): Stream[Long] = n #:: alstream(sumpropdiv(n))

    val (maxlen, elem) = Stream.from(2).takeWhile(_ <= ceiling).map(amicableLength(_)).maxBy(_._1)

    val minelem = alstream(elem).take(maxlen).min

    //println(amicableLength(1188))
    //(2 to 10000).foreach(x => println(x, amicableLength(x)))
    println(minelem)

  }

  def problem94() = {
    val ceiling = 1000000000l
    val maxside = (ceiling / 3) + 1
    var sum = 0l

    for {
      ab <- longsFrom(3, 2).takeWhile(_ <= maxside)
      c <- List(ab - 1, ab + 1)
      if isSquare(ab * ab - c * c / 4)
    } sum += 2 * ab + c

    println(sum)
  }

  def problem93() = {
    val ops: Set[(Double, Double) => Double] = Set(_ + _, _ - _, _ * _, _ / _)

    def exprvals(digits: List[Int]): Set[Int] = {

      def iter(digits: List[Int]): Set[Double] =
        if (digits.tail.isEmpty) Set(digits.head)
        else for {
          op <- ops
          num <- iter(digits.tail)
          exprval <- List(op(num, digits.head), op(digits.head, num))
        } yield exprval

      // filter for positive integers
      iter(digits).filter(n => n % 1 == 0 && n > 0).map(_.toInt)
    }

    val res = for {
      i <- 0 to 6
      j <- i + 1 to 7
      k <- j + 1 to 8
      l <- k + 1 to 9
      comb = List(i, j, k, l)
      combvals = comb.permutations.foldLeft(Set[Int]())((acc, perm) =>
        acc union exprvals(perm))
      max = combvals.toList.sorted.zipWithIndex.indexWhere({ case (v, i) => v != i + 1 })
      adjmax = if (max == -1) combvals.size else max
    } yield (adjmax, comb)

    println(res.maxBy(_._1))
  }

  def problem92() = {
    @tailrec
    def numberChain(n: Long): Long = {
      val next = digits(n).foldLeft(0)((sumsq, digit) => sumsq + digit * digit)
      if (next == 1 || next == 89) next else numberChain(next)
    }

    println((1 until 10000000).count(numberChain(_) == 89))
  }
  //time(problem91())
  def problem91() = {
    def isRightTriangle(asq: Int, bsq: Int, csq: Int) =
      (asq == bsq + csq) || (bsq == asq + csq) || csq == asq + bsq

    def countTriangles(side: Int) = {
      var count = 0
      for {
        px <- 0 to side
        py <- 0 to side
        qx <- 0 to side
        qy <- 0 to side
        crossprod = px * qy - qx * py // check co-linearity
        if crossprod != 0
        psq = px * px + py * py
        qsq = qx * qx + qy * qy
        pqsq = (px - qx) * (px - qx) + (py - qy) * (py - qy)
        if isRightTriangle(psq, qsq, pqsq)
      } {
        count += 1
      }
      count / 2 // when p and q are swapped only count one triangle
    }

    println(countTriangles(50))
  }

  def problem90() = {
    val diefaces = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val squares = List("01", "04", "09", "16", "25", "36", "49", "64", "81").map(_.toList.map(_.asDigit))

    def diesmatch(die1: List[Int], die2: List[Int], sqdigits: List[Int]) = {
      val die1ext = if ((List(6, 9) diff die1).size < 2) die1 union List(6, 9) else die1
      val die2ext = if ((List(6, 9) diff die2).size < 2) die2 union List(6, 9) else die2
      sqdigits match {
        case List(d1, d2) => die1ext.contains(d1) && die2ext.contains(d2) ||
          die1ext.contains(d2) && die2ext.contains(d1)
      }
    }

    var count = 0
    for {
      die1 <- diefaces.combinations(6)
      die2 <- diefaces.combinations(6)
      if (squares.forall(diesmatch(die1, die2, _)))
    } {
      count += 1
    }

    println(count / 2)
  }

  def problem89() = {
    val rnums = Source.fromFile("resources/roman.txt").mkString.split("\n").toList

    val rnmap = Map('I' -> 1, 'V' -> 5, 'X' -> 10, 'L' -> 50, 'C' -> 100, 'D' -> 500, 'M' -> 1000)

    def rnFormat(n: Int): String = {
      def formatDecade(buf: StringBuilder, n: Int, pten: Int, one: String, five: String, ten: String) = {
        val nunits = (n % (pten * 10)) / pten
        if ((nunits + 1) % 5 == 0) {
          buf ++= one + (if (nunits == 4) five else ten)
        } else {
          if (nunits >= 5) buf ++= five
          buf ++= one * (nunits % 5)
        }
      }

      val buf = new StringBuilder
      buf ++= "M" * (n / 1000)
      var nhundreds = (n % 1000) / 100

      formatDecade(buf, n, 100, "C", "D", "M")
      formatDecade(buf, n, 10, "X", "L", "C")
      formatDecade(buf, n, 1, "I", "V", "X")

      buf.toString
    }

    def rnParse(rn: String): Int = {
      if (rn.length == 1) rnmap(rn.head)
      else {
        var skipNext = false
        (rn.sliding(2).map(p => {

          if (skipNext) {
            skipNext = false
            0
          } else {
            val n0 = rnmap(p(0))
            val n1 = rnmap(p(1))
            if (n0 < n1) {
              skipNext = true
              n1 - n0
            } else n0
          }
        }) ++ (if (skipNext) Nil else List(rnmap(rn.last)))).sum
      }
    }

    rnums.map(rn => (rn, rnParse(rn), rnFormat(rnParse(rn)), rnParse(rnFormat(rnParse(rn))))).foreach(println)

    println(rnums.map(_.length).sum - rnums.map(s => rnFormat(rnParse(s)).length).sum)
  }

  def problem88() = {

    val ceiling = 12000
    val prodsum = collection.mutable.HashMap[Int, Int]()
    var complete = false

    for {
      n <- Stream.from(2).takeWhile(_ => !complete)
      fp <- factorproducts(n)
      if (fp.head != n)
      nelems = (fp.size + n - fp.sum).toInt
      if !prodsum.contains(nelems)
    } {
      if (nelems <= ceiling) {
        prodsum(nelems) = n
        if (prodsum.size >= ceiling - 1) complete = true
      }
    }

    println(prodsum.size, prodsum.values.toList.distinct.sum)
  }

  def problem87() = {
    val ceiling = 5e7
    val maxc = pow(ceiling, 0.25).toInt

    val count = (for {
      c <- primes.takeWhile(_ <= maxc)
      c4 = c * c * c * c
      maxb = pow(ceiling - c4, 1.0 / 3).toInt
      b <- primes.takeWhile(_ <= maxb)
      b3 = b * b * b
      maxa = sqrt(ceiling - c4 - b3).toInt
      a <- primes.takeWhile(_ <= maxa)
      a2 = a * a
    } yield (a2 + b3 + c4)).toSet.size

    println(count)
  }

  def problem86() = {
    val ceiling = 1000000
    var count = 0
    def combs(a: Int, bc: Int) = {
      val amax = bc / 2
      val bmax = min(bc - 1, a)
      val amin = bc - bmax
      amax - amin + 1
    }

    for {
      a <- Stream.from(1).takeWhile(_ => count < ceiling)
      bc <- 2 to 2 * a
      len = sqrt(a * a + bc * bc)
      if len % 1 == 0
    } {
      count = count + combs(a, bc)
      if (count > ceiling) println(a)
    }
  }

  def problem85() = {
    val target = 2000000
    var mindiff = (0, 0, target)
    def solveQuadratic(n: Int): Double = (-1 + sqrt(1 + 16 * target / (n * (n + 1)))) / 2
    val res = for {
      i <- 1 to solveQuadratic(1).toInt
      targetj = solveQuadratic(i).toInt
      j <- targetj to targetj + 1
      count = i * (i + 1) * j * (j + 1) / 4
    } {
      if (mindiff._3 > abs(target - count)) mindiff = (i, j, abs(target - count))
    }
    println(mindiff._1 * mindiff._2, mindiff)
    println(solveQuadratic(1))
  }
  //println(res)
  // problem84()
  def problem84() = {
    val GO = 0 // index of the 'Go' square
    val JAIL = 10 // index of the 'Jail' square
    val GOTOJAIL = 30 // index of the 'Go To Jail' square

    /** Represents a Monopoly Board Square */
    class Square {
      /**
       * If we land on this square with probability 'prob' after a dice throw, this function
       * decides how that probability gets distributed over this and other squares due to
       * possible further moves (e.g. if this is a Community Chest or Chance square).
       */
      def distribute(pos: Int, prob: BigDecimal): List[(Int, BigDecimal)] = List((pos, prob))
    }

    case object CommunityChestSquare extends Square {
      override def distribute(pos: Int, prob: BigDecimal) = CommunityChestDeck.distribute(pos, prob)
    }

    case object ChanceSquare extends Square {
      override def distribute(pos: Int, prob: BigDecimal) = ChanceDeck.distribute(pos, prob)
    }

    /**
     * Represents a Community Chest or Chance card.
     * The movePos function returns the end position implied by this card, for the starting position
     * provided as an argument.
     */
    case class Card(val movePos: (Int) => Int)

    /** Represents a deck of (Community Chest or Chance) cards.  */
    abstract class Deck {
      /** Total number of cards in this deck, including non-move cards. */
      val ncards: Int

      /** List of cards with further move instructions */
      val movecards: List[Card]

      /**
       *  Distribute the probability of landing on this square across the starting square, and
       *  the squares that the cards in this deck may give instructions to move to (including possible
       *  secondary moves if the first move also takes us to square which can lead to possible further moves).
       *  We make the simplifying assumption that the deck remains continuously randomly shuffled.
       */
      def distribute(pos: Int, prob: BigDecimal) = {
        (pos, prob * (ncards - movecards.size) / ncards) ::
          movecards.flatMap(c => {
            val endpos = c.movePos(pos)
            val endsquare = Board(endpos)
            endsquare.distribute(endpos, prob / ncards)
          }).toList
      }
    }

    /**
     * Represents the Community Chest card deck.
     * Note that for the 'Go To Jail' card, we redirect to the 'Go To Jail' square rather than directly to the 'Jail'
     * square, which allows the iteration logic to correctly handle resetting the double counter to zero.
     */
    object CommunityChestDeck extends Deck {
      val ncards = 16
      val movecards =
        Card(_ => GO) :: // go to GO
          Card(_ => GOTOJAIL) :: // go to Jail
          Nil
    }

    /**
     * Represents 'Chance' card deck.
     * Note that for the 'Go To Jail' card, we redirect to the 'Go To Jail' square rather than directly to the 'Jail'
     * square, which allows the iteration logic to correctly handle resetting the double counter to zero.
     */
    object ChanceDeck extends Deck {
      val ncards = 16
      val movecards =
        Card(_ => GO) :: // go to GO
          Card(_ => GOTOJAIL) :: // go to JAIL 
          Card(_ => 11) :: // go to C1
          Card(_ => 24) :: // go to E3
          Card(_ => 39) :: // go to H2
          Card(_ => 5) :: // go to R1
          Card(pos => (pos + 10 - (pos + 5) % 10) % Board.nsquares) :: // go to next R
          Card(pos => (pos + 10 - (pos + 5) % 10) % Board.nsquares) :: // go to next R 
          Card(pos => if (pos < 28 && pos >= 12) 28 else 12) :: // go to next U
          Card(pos => (pos + 37) % Board.nsquares) :: // go back 3
          Nil
    }

    /** Representation of the Monopoly Board. */
    object Board {
      val nsquares = 40
      val squares: Array[Square] = Array.fill(nsquares)(new Square())
      squares(2) = CommunityChestSquare
      squares(7) = ChanceSquare
      squares(17) = CommunityChestSquare
      squares(22) = ChanceSquare
      squares(33) = CommunityChestSquare
      squares(36) = ChanceSquare

      def apply(i: Int) = squares(i)
    }

    /**
     * Simulate a dice role of two dice with the given number of sides.
     * Returns a list of ordered pairs of dice numbers alongside the probability of obtaining those
     * numbers (in either order). This representation allows us to retain information on whether
     * a double was thrown.
     */
    def rollDice(sides: Int): Seq[(Int, Int, BigDecimal)] = {
      val sidesSq: Double = sides * sides
      for {
        i <- 1 to sides
        j <- i to sides
        n = (if (i == j) 1 else 2)
      } yield {
        (i, j, BigDecimal(n) / sidesSq)
      }
    }

    /**
     * Simulation of a monopoly game.
     *
     * I represent the board state probability distribution with a 40 x 3 array - the three element subarray keeps
     * track of the marginal probability distributions where 0, 1 and 2 consecutive doubles have been thrown. The sum
     * of all elements in the entire 40 x 3 array should add up to one. The probability of ending on any one square at
     * the end of a roll is then the sum of the probabilities in the 3 element subarray for that square.
     */
    object Simulation {
      /**
       * Data structure tracking the probability distribution over all squares, and for numbers of consecutive
       * double dice throws between 0 and 2 inclusive (the second array index represents the number of consecutive
       * doubles thrown).
       */
      val probdist = Buffer.fill[BigDecimal](Board.nsquares, 3)(0)

      val NSIDES = 6 // number of sides on the die

      /* Cache the dice roll probability distribution, as it's the same for all iterations */
      val rolls = rollDice(NSIDES)

      probdist(0)(0) = 1.0 // start off the iteration in the GO square with no previous doubles thrown

      /**
       * Execute one iteration of the simulation, returning the sum of all changes to probability values across the
       * entire array made during this iteration.
       *
       * In the iteration we cycle through each square on the board, and redistribute the probability assigned that
       * square by the previous iteration to other squares, according to dice throw probability distribution, and
       * all the other rules affecting how moves are made. If and when the iteration converges, the probability
       * flowing into each square from all other squares will exactly replace the probability flowing out of it
       * (i.e. the probability it started with).
       */
      def iter(): BigDecimal = {
        /* 
       * Probability distribution changes made during this iteration should be based on the transition 
       * probabilities present at the *start* of the iteration, so we need to make a static copy of the initial
       * distribution. This is also need to calculate the return value of summed changes.
       */
        val initprobdist = probdist.toList.map(_.toList)

        for {
          /* iterate over number of previous consecutive double throws */
          ndbl <- 0 to 2
          (startsquare, startpos) <- Board.squares.zipWithIndex // the starting position
          startprob = initprobdist(startpos)(ndbl) // probability associated with the start square, to be redistributed

          /* iterate over dice rolls */
          (d1, d2, rollprob) <- rolls
          initendpos = (startpos + d1 + d2) % 40 // the initial end position immediately after the dice roll 
          initendsquare = Board.squares(initendpos)

          /* 
         * iterate over the possible secondary end states, e.g. if the initial end state could lead to further 
         * move instructions
         */
          (endpos, endprob) <- initendsquare.distribute(initendpos, startprob * rollprob)
        } {

          /* 
         * Subtract the probability we are about to redistribute from the start square probability 
         * (since probability redistribution is a zero sum game)
         */
          probdist(startpos)(ndbl) -= endprob

          /*
         * Handle 'Go To Jail'. We could be here because the 'Go To Jail' square was landed on,
         * *or* the 'Go To Jail' Chance or Community Chest card was selected. In either case, we
         * assign the transition probability to the 'zero doubles thrown' slot of the Jail square 
         * (i.e. the double counter gets reset to zero when you're sent to Jail).
         */
          if (endpos == GOTOJAIL) {
            probdist(JAIL)(0) += endprob
            //if (ndbl == 2 && d1 == d2) probdist(JAIL)(0) += endprob
            //else probdist(JAIL)(ndbl) += endprob
          } /*
         * Handle a double being thrown.
         */ else if (d1 == d2) {
            /* Go to Jail if this is the third consecutive double throw */
            if (ndbl == 2) probdist(JAIL)(0) += endprob
            /* otherwise assign the transition probability to incremented double count slot of end square */
            else probdist(endpos)(ndbl + 1) += endprob
          } /* Normal case - just add the transition probability zero double count slot of the end square. No 
         * double was thrown so any previous double count is reset to zero.*/ else probdist(endpos)(0) += endprob
        }

        /* 
       * Return the summed difference over all elements between the probability distributions at the start and
       * end of this iteration.
       */
        (initprobdist, probdist).zipped.map((a, b) => (a, b).zipped.map((x, y) => (x - y).abs).sum).sum
      }

      /*
    def normalize() = {
      val scalefactor = 1.0 / probs.flatMap(identity).sum
      probs.transform(_.transform(_ * scalefactor))
    }
    */

      /**
       * Run the iteration until the maximum change is less than the chosen threshold, and print the results.
       */
      def run() = {
        var maxdelta = BigDecimal(1.0e-19)
        var delta = BigDecimal(1.0)
        var niter = 0

        while (delta > maxdelta) delta = { niter += 1; iter() }

        val totalprob = probdist.map(_.sum)
        // print the top three squares by probability
        println("Completed in " + niter + " iterations.")
        totalprob.zipWithIndex.sortBy({ case (p, i) => -p }).take(3).foreach(p => println(p._2 + ": " + p._1))
      }
    }

    time(Simulation.run())
  }

  def problem83() = {
    val matrix = Source.fromFile("resources/matrix.txt").mkString.split("\n").map(_.split(",").map(_.toInt))
    val size = matrix.size
    val buffer = Buffer.fill(size, size)(-1)
    buffer(0)(0) = matrix(0)(0)
    val coordRange = (0 until size)
    val visited = collection.mutable.HashSet[(Int, Int)]()
    val touched = collection.mutable.HashSet[(Int, Int)]()

    def shortestPath(): Int = {

      def neighbours(current: (Int, Int)): List[(Int, Int)] = current match {
        case (x, y) => List((x, y + 1), (x + 1, y), (x - 1, y), (x, y - 1))
      }

      def legalNeighbours(current: (Int, Int)): List[(Int, Int)] =
        neighbours(current).filter({
          case (x, y) =>
            coordRange.contains(x) && coordRange.contains(y) && !visited.contains((x, y))
        })

      def findNewCurrent(): (Int, Int) = touched.minBy({ case (x, y) => buffer(x)(y) })

      def iter(current: (Int, Int)): Int = {
        val (x, y) = current
        for {
          (nx, ny) <- legalNeighbours(current)
        } {
          val newVal = buffer(x)(y) + matrix(nx)(ny)
          val oldVal = buffer(nx)(ny)
          if (oldVal == -1 || newVal < oldVal) buffer(nx)(ny) = newVal
          touched += ((nx, ny))
        }
        visited += ((x, y))
        touched -= ((x, y))
        val newCurrent = findNewCurrent()
        if (newCurrent == (size - 1, size - 1)) buffer(size - 1)(size - 1)
        else iter(newCurrent)
      }

      iter((0, 0))
    }

    println(shortestPath())
  }

  def problem82() = {
    val matrix = Source.fromFile("resources/matrix.txt").mkString.split("\n").map(_.split(",").map(_.toInt))
    val size = matrix.size
    val buffer = Buffer.tabulate(size, size)((i, j) => matrix(j)(size - i - 1))

    for (i <- 1 to size - 1; j <- 0 to size - 1) {
      buffer(i)(j) = (for (k <- 0 to size - 1) yield (min(j, k) to max(j, k)).map(matrix(_)(size - i - 1)).sum + buffer(i - 1)(k)).min
    }

    println(buffer.last.min)
  }

  def problem81() = {
    val matrix = Source.fromFile("resources/matrix.txt").mkString.split("\n").map(_.split(",").map(_.toInt).toBuffer[Int].reverse).reverse

    val size = matrix.size
    for {
      i <- 0 to size - 1
      j <- 0 to size - 1
    } {
      matrix(i)(j) += {
        if (i == 0 && j == 0) 0
        else if (i == 0) matrix(i)(j - 1)
        else if (j == 0) matrix(i - 1)(j)
        else min(matrix(i - 1)(j), matrix(i)(j - 1))
      }
    }

    println(matrix(size - 1)(size - 1))
  }

  def problem80() = {
    val delta = BigDecimal(1, 101)
    def sqrtNewton(n: BigDecimal): BigDecimal = {
      def isCloseEnough(r: BigDecimal) = {
        (r * r - n).abs / n <= delta
      }

      def nextGuess(r: BigDecimal) = (r + n / r) / 2

      def iter(r: BigDecimal): BigDecimal =
        if (isCloseEnough(r)) r else iter(nextGuess(r))

      iter(n / 2)
    }

    val mc = new MathContext(102)
    val res = (1 to 100).filter(n => !isSquare(n)).map(n => {
      val bd = BigDecimal(n, mc)
      val root = sqrtNewton(bd)
      digits(root).take(100).sum
    }).sum
    println(res)
  }

  def problem79() = {
    val passcodes = Source.fromFile("resources/keylog.txt").mkString.split("\n")

    var password = ""
    var index = -1

    for {
      passcode <- passcodes.toStream
      _ = index = -1
      digit <- passcode
      _ = if (password.contains(digit)) {
        val charIndex = password.indexOf(digit)
        if (charIndex <= index) {
          val (start, end) = password.splitAt(index + 1)
          password = start.replace(digit.toString, "") + digit + end
        }
      } else password = password + digit
      _ = index = password.indexOf(digit)
    } {}

    println(password)

  }

  def problem78() = {

    val pcache = collection.mutable.HashMap[Long, BigInt](0l -> BigInt(1))

    def genpent(n: Int): Long = {
      val k = (n + 1) / 2
      val sign = -1 + ((n % 2) * 2)
      val sk = k * sign
      sk * (3 * sk - 1) / 2
    }

    def p(n: Int): BigInt = {
      pcache.getOrElseUpdate(n, {
        var sum = BigInt(0);
        for {
          (k, pent) <- Stream.from(1).map(k => (k, genpent(k))).takeWhile(_._2 <= n)
          sign = if (((k + 1) / 2) % 2 == 0) -1 else 1
          term = sign * pcache(n - pent)
        } { sum += term }
        sum
      })
    }

    val res = Stream.from(1).find(p(_) % 1000000 == 0).get
    println(res, p(res))

    /*
  val cache = collection.mutable.HashMap[(Long, Long), BigInt]()

  def p(k: Long, n: Long): BigInt = {
    val key = (k, n)
      cache.getOrElse(key, {
      val v = if (k > n) BigInt(0)
      else if (k == n) BigInt(1)
      else p(k + 1, n) + p(k, n - k)
      cache(key) = v
      v
    })
  }
  def pprim(n:Long) = {println("pprim", n); (BigInt(1) /: (1l to n / 2)) { (acc, k) => acc + p(k, n - k) } }   
  */
  }

  def problem77() = {

    def countPrimeSums(n: Long): Long = {
      def iter(n: Long, ceil: Long): Long =
        if (n < 0) 0
        else if (n == 0) 1
        else (0L /: primes.takeWhile(_ <= ceil)) { (acc, p) => acc + iter(n - p, p) }

      iter(n, n - 1)
    }

    val res = Stream.from(1).find(countPrimeSums(_) > 5000)
    println(res.get)
  }

  def problem76() = {
    val cache = collection.mutable.Map[(Int, Int), Long]()
    def countSum(n: Int): Long = {
      def iter(n: Int, ceil: Int): Long =
        cache.getOrElse((n, ceil), {
          cache((n, ceil)) =
            if (n <= 1) 1
            else (0L /: (1 to min(n, ceil))) { (acc, i) => acc + iter(n - i, i) }
          cache((n, ceil))
        })
      iter(n, n - 1)
    }

    println(countSum(100))
  }

  def problem75b() = {
    val ceiling = 1500000
    var result = 0
    val triangles = new Array[Int](ceiling + 1)

    val mmax = ((-1 + sqrt(1 + 2 * ceiling)) / 2).toInt
    //println(mmax, sqrt(ceiling /2), sqrt(ceiling))
    for {
      m <- 2 to mmax
      nmin = if (m % 2 == 0) 1 else 2
      nmax = min(m - 1, (ceiling - 2 * m * m) / (2 * m))
      n <- nmin to nmax by 2
      if gcf(m, n) == 1
      pprim = 2 * (m * m + m * n)
      k <- 1 to ceiling / pprim
      p = pprim * k
    } {
      triangles(p) += 1
      if (triangles(p) == 1) result += 1;
      if (triangles(p) == 2) result -= 1;
    }
    println(result)
  }

  def problem75() = {
    val ceiling = 1500000
    var rescount = 0
    for {
      p <- (2 to ceiling by 2).toStream
      mmax = ((sqrt(1 + 2 * p) - 1) / 2).toInt
      //mmin = -3  + sqrt(9 + 2 * p) / 2
      count = (for {
        m <- 2 to mmax
        twomsq = 2 * m * m
        nmax = (p - twomsq) / (2 * m)
        //_ = println(m,mmax,nmax, p)
        n <- 1 to ((p - twomsq) / (2 * m))
        if m > n //&& gcf(m, n) == 1
        k = 1.0 * p / (twomsq + 2 * m * n)
        if isWhole(k)
        kint = k.toInt
        a = (m * m - n * n) * kint
        b = (2 * m * n) * kint
        //c = (m *m + n * n) * kint
        //_ = println(a,b,c, p, m, n, kint)
      } yield if (a < b) (a, b) else (b, a)).distinct.size
      if count == 1
    } { rescount += 1; if (rescount % 1000 == 0) println(rescount, p) }

    println(rescount)
    //   (2 to mmax).map(m => (1.0 * p - 2 * m * m) / (2 * m))
    //  _ = if (nmax == nmax.toInt) count += 1    
    //} 

  }

  def problem74() = {
    def digits(n: Any) = n.toString.map(_.asDigit)

    def digitfact(n: Long): Long = {
      digits(n).map(fact(_)).sum
    }

    def countfactchain(n: Long): Int = {
      def iter(n: Long, prev: List[Long]): Int = {
        val next = digitfact(n)
        if (prev.contains(next)) 0
        else 1 + iter(next, next :: prev)
      }
      1 + iter(n, List())
    }

    var count = 0
    for {
      n <- 1 until 1000000
      if countfactchain(n) == 60
    } count += 1

    println(count)
  }

  def problem73() = {
    val ceiling = 12000
    var count = 0
    for {
      d <- 2 to ceiling
      n <- d / 3 + 1 to d / 2
      if (n * 3 > d && n * 2 < d && gcf(n, d) == 1)
    } count += 1

    println(count)
  }

  def problem72() = {
    val ceiling = 1000000
    var count = 0L
    for {
      d <- (2 to ceiling)
    } {
      count += Phi.phi(d)
    }

    println(count)
  }

  def problem71 = {
    val ceiling = 1000000
    var maxfrac = ~~(0, 1)
    for {
      d <- (2 to ceiling).toStream
      n = 3 * d / 7
      //_ = {println (n, d)}
      //if gcf(n, d) == 1
      frac = ~~(n, d)
      if (frac > maxfrac && 7 * n < 3 * d)
      //_ = println(n, d, frac, maxfrac)
    } maxfrac = frac

    println(maxfrac)
  }

  def problem70() = {
    val ceiling = 10000000
    val rootceiling = sqrt(10000000)
    var nOverPhiN_min = 100.0
    val maxPrimeIndex = Stream.from(1).indexWhere(Prime(_) > rootceiling) - 1
    val res = for {
      i <- Stream.from(maxPrimeIndex - 1, -1).takeWhile(_ > 0)
      pi = Prime(i)
      alpha = nOverPhiN_min * (pi - 1) / pi
      pjlimit = alpha / (alpha - 1)
      pj <- Stream.from(i + 1).map(Prime(_))
        .dropWhile(p => if (pjlimit > 0) p < pjlimit else false)
        .takeWhile(p => if (pjlimit < 0) p <= pjlimit else true && p * pi < ceiling)
      n = pi * pj
      phin = (pi - 1) * (pj - 1)
      if ispermutation(n, phin)
      nOverPhiN = 1.0 * n / phin
      _ = if (nOverPhiN_min > nOverPhiN) { nOverPhiN_min = nOverPhiN }
    } yield (n, pi, pj, nOverPhiN)

    println(res.minBy(_._4))
  }

  def problem69b() = {
    var prod = 1.0
    breakable {
      for {
        p <- primes
        _ = { prod = prod * p }
        if prod < 1000000 || { prod = prod / p; break; false }
      } {}
    }
    println(prod)
  }

  def problem69() = {
    var maxNdivPhi = (0, 0.0)
    for {
      n <- 2 to 1000000
      _ = if (n % 1000 == 0) println(n)
      nDivPhi = 1.0 * n / Phi.phi(n)
    } if (maxNdivPhi._2 < nDivPhi) maxNdivPhi = (n, nDivPhi)

    println(maxNdivPhi)
  }

  def problem68() = {
    def data(n: Int) = (1 to 2 * n - 1).permutations.map(x => (2 * n +: x.toList).grouped(2).toList)

    val res = for {
      mapping <- data(5)
      firstpair = mapping.head
      lastpair = mapping.last
      tripletspartial = for (List(first, second) <- mapping.sliding(2))
        yield List(first(0), first(1), second(1))
      triplets = List(lastpair(0), lastpair(1), firstpair(1)) :: tripletspartial.toList
      if triplets.sliding(2).forall({ case List(x, y) => x.sum == y.sum })
    } yield {
      val (start, end) = triplets.splitAt(triplets.indexOf(triplets.minBy(_(0))))
      (end ++ start).flatMap(x => x).mkString.toLong
    }

    println(res.max)
  }

  def problem67() = {
    val tridata = Source.fromFile("resources/triangle.txt").mkString.split("\n").map(_.split(" ").map(_.toInt).toList).toList

    def maxPairs(l: List[Int]): List[Int] = l.sliding(2).map(_.max).toList

    var rowbuf = List.fill(tridata.size)(0)
    for (row <- tridata.reverse) rowbuf = maxPairs((rowbuf zip row).map(x => x._1 + x._2))

    println(rowbuf.head)
  }

  def problem66() = {
    val res = for {
      d <- (1 to 1000).filter(!isSquare(_))
      pellSln = sqrtContinuedFrac(d).dropWhile({ case (r, s) => r * r - d * s * s != 1 }).head
    } yield (d, pellSln)

    res.foreach(println)
    println(res.maxBy(_._2._1) match { case (n, (r, s)) => (n, r, s, r * r - 661 * s * s) })
  }

  def problem65() = {
    def biggcf(v1: BigInt, v2: BigInt): BigInt = {
      val (smaller, larger) = if (v1 < v2) (v1, v2) else (v2, v1)
      if (smaller == 0) larger
      else if (larger % smaller == 0) smaller
      else biggcf(smaller, larger - smaller)
    }

    def ecf(): Stream[Long] = {
      def iter(n: Long): Stream[Long] = 1 #:: (2 * n) #:: 1 #:: iter(n + 1)
      2 #:: iter(1)
    }

    println(digits(continuedFrac(100, ecf)._1).sum)
  }

  def problem64() = {
    println((2 to 10000).filter(x => !isSquare(x) && sqrtContinuedFracCoeffs(x, true).size % 2 == 0).size)
  }

  def problem63() = {
    val res = for {
      power <- (1 to 22).toSet[Int]
      n <- 1 to 9
      testnum = BigInt(n).pow(power)
      if digits(testnum).size == power
    } yield testnum

    println(res.size)
  }

  def problem62() = {
    def cubeStream(): Stream[Long] = {
      def str(n: Long): Stream[Long] = (n * n * n) #:: str(n + 1)
      str(0)
    }

    val cubemap = collection.mutable.Map[Map[Int, Int], List[Long]]().withDefaultValue(List())
    breakable {
      for {
        cube <- cubeStream()
        sig = digits(cube).sorted.groupBy(x => x).mapValues(_.size)
        _ = cubemap(sig) = cube :: cubemap(sig)
        if cubemap(sig).size == 5
      } { println(cubemap(sig).min); break }
    }
  }

  def problem61() = {

    def digits(n: Any) = n.toString.map(_.asDigit)

    def fourdigitnum(f: Long => Long) = Stream.from(1).map(f(_)).dropWhile(x => digits(x).size < 4).takeWhile(x => digits(x).size == 4)

    val octs = fourdigitnum(octagon).toList
    val hepts = fourdigitnum(heptagon).toList
    val hexs = fourdigitnum(hexagon).toList
    val pents = fourdigitnum(pentagon).toList
    val squares = fourdigitnum(square).toList
    val triangles = fourdigitnum(triangle).toList

    val groups = List(triangles, squares, pents, hexs, hepts, octs)

    def startsWith(prefix: Long, n: Long) = {
      val prefixDigits = digits(prefix)
      digits(n).take(prefixDigits.length) == prefixDigits
    }

    def extend(paths: List[List[(Long, Int)]]) = {
      for {
        path <- paths
        (v, n) = path.head
        (g, _gi) <- groups.zipWithIndex
        gi = _gi + 3
        gv <- g
        if v % 100 > 9 && startsWith(v % 100, gv)
        if path.forall({ case (v, n) => n != gi })
      } yield (gv, gi) :: path
    }

    val res = (1 to 5).foldLeft(octs.map(o => List((o, 8))))((x, y) => extend(x))
    println(res.filter(l => l.head._1 % 100 > 9 && startsWith(l.head._1 % 100, l.last._1)).head.map(_._1).sum)
  }

  def problem60() = {
    var cache = collection.mutable.Map[Long, Boolean]()

    def primeCombs(primes: Long*) = {
      val firststr = primes.head.toString
      primes.tail.forall(p => {
        val prod = p * primes.head
        cache.getOrElse(prod, {
          cache(prod) = Prime.isPrime((p.toString + firststr).toLong) &&
            Prime.isPrime((firststr + p.toString).toLong)
          cache(prod)
        })
      })
    }

    val MAX = 30000

    var solution: Long = Long.MaxValue
    breakable {
      for {
        ceiling <- Stream.from(1).map(pow(2, _))
        prevceiling = ceiling / 2
        pstream = primes.dropWhile(_ <= 2).takeWhile(_ <= ceiling)
        p1 <- pstream
        _ = println(p1, solution, ceiling)
        _ = if (p1 > solution) break
        p2 <- pstream.dropWhile(_ <= p1).takeWhile(solution - p1 - _ >= 0)
        if primeCombs(p2, p1)
        p3 <- pstream.dropWhile(_ <= p2).takeWhile(solution - p1 - p2 - _ >= 0)
        if primeCombs(p3, p2, p1)
        p4 <- pstream.dropWhile(_ <= p3).takeWhile(solution - p1 - p2 - p3 - _ >= 0)
        if primeCombs(p4, p3, p2, p1)
        p5 <- pstream.dropWhile(x => x <= p4 && x >= prevceiling).takeWhile(solution - p1 - p2 - p3 - p4 - _ >= 0)
        if primeCombs(p5, p4, p3, p2, p1)
        sum = p1 + p2 + p3 + p4 + p5
      } {
        if (sum < solution) solution = sum
      }

    }
    println(solution)
  }

  /*
  var result:List[Long] = null
  
  val pstream = Prime.stream().dropWhile(_ <= 2)
  breakable {
    for {
      p1 <- pstream
      _ = println(p1)
      p2 <- pstream.takeWhile(_ <= p1)
      p3 <- pstream.takeWhile(_ <= p2)
      p4 <- pstream.takeWhile(_ <= p3)
      p5 <- pstream.takeWhile(_ <= p4)
      plist = List(p1, p2, p3, p4, p5)
      //_ = {if (p1 % 100 < 5) println(plist)}
      if plist.combinations(2).forall(comb => comb.permutations.forall(x => Prime.isPrime(x.mkString.toLong)))
    } {
      result = plist; break
    }
  }
  
  println(result)
  */

  def problem59() = {
    def decode(bytes: List[Char], key: List[Char]): List[Char] = {
      for {
        i <- (0 until bytes.length).toList
        keychar = key(i % key.size)
      } yield (bytes(i) ^ keychar).toChar
    }

    val bytes = Source.fromFile("resources/cipher1.txt").mkString.split(",").map(_.trim.toInt.toChar).toList
    val res = for {
      x <- 'a' to 'z'
      y <- 'a' to 'z'
      z <- 'a' to 'z'
      message = decode(bytes, List(x, y, z)).mkString
      lmessage = message.toLowerCase()
      if "the".r.findAllIn(lmessage).length > 6 && "and".r.findAllIn(lmessage).length > 6
    } yield message

    res.foreach(msg => println(msg.map(_.toInt).sum, msg))
  }

  def problem58() = {
    def spirdiag(n: Long) = {
      if (n <= 1) 1 else {
        val cycle = (n + 2) / 4
        val base = 4 * cycle * cycle - 2 * cycle + 1
        base + ((n + 2) % 4) * cycle * 2
      }
    }

    var primeCount: (Int, Int, Int) = (0, 0, 0)

    breakable {
      val primes = Stream.from(1, 2).foldLeft((0, 0, 0))((cum, n) =>
        cum match {
          case (prevn, prevp, prevnp) => {
            val nprimes = (2 * n - 4 to 2 * n - 1).foldLeft(0)((cum, n) => if (Prime.isPrime(spirdiag(n))) cum + 1 else cum)
            primeCount = (n, prevp + nprimes, prevnp + 4 - nprimes)
            if (primeCount._2 != 0 && (primeCount._3 + primeCount._2).toDouble / primeCount._2 > 10.0) break
            primeCount
          }
        })
    }

    println(primeCount)
  }

  def problem57() = {
    def iter(frac: (BigInt, BigInt)): (BigInt, BigInt) = frac match {
      case (a, b) => (2 * b + a, b + a)
    }

    println((1 to 1000).scanLeft((BigInt(1), BigInt(1)))((x, y) => iter(x)).count({ case (a, b) => digits(a).size > digits(b).size }))
  }

  def problem56() = {
    println((for {
      a <- BigInt(1) to 99
      b <- 1 to 99
      c = a.pow(b)
    } yield digits(c).sum).max)
  }

  def problem55() = {
    def lychrelOp(n: BigInt): BigInt = n + BigInt(digits(n).reverse.mkString)

    def isLychrel(n: BigInt) = {
      var running: BigInt = n
      (1 to 50).forall(i => { running = lychrelOp(running); !isPalindrome(running) })
    }

    println((BigInt(1) to 10000).count(x => isLychrel(x)))
  }

  def problem54() = {
    val source = Source.fromFile("resources/poker.txt")

    var count = 0
    val hands = for {
      line <- source.getLines
      List(p1hand, p2hand) = line.split("""\s+""").grouped(5).toList.map(new Hand(_))
      if (p1hand > p2hand)
      //_ = {println(p1hand, "\t", p2hand, p1hand > p2hand,p1hand.handRank, p2hand.handRank)}
    } count += 1
    println(count)
  }

  def problem53() = {
    var sum = 0L
    for {
      n <- 10 to 100
      r <- 1 to 100
      if ncr(n, r) > 1000000
    } { sum += 1 }
    println(sum)
  }

  def problem52() = {
    breakable {
      for {
        n <- Stream.from(1)
        nsets = (1 to 6).map(x => digits(x * n).toSet).distinct.size
        if nsets == 1
      } { println(n); break }
    }
  }

  def problem51() = {
    breakable {
      for {
        p <- primes.dropWhile(_ < 1000)
        p3digits = digits(p).groupBy(x => x).filter({ case (num, reps) => num <= 2 && reps.size >= 3 })
        if !p3digits.isEmpty
        (repNum, _) <- p3digits
        combs <- variantNums(p, repNum, 3)
        if combs.count(p => Prime.isPrime(p)) >= 8
      } {
        println(p)
        break
      }
    }

    def variantNums(basePrime: Long, repeatedNum: Int, repetitions: Int) = {
      val basePrimeDigits = digits(basePrime)

      val combinations = basePrimeDigits.zipWithIndex.filter(_._1 == repeatedNum).map(_._2).combinations(repetitions).toList

      val variants = for {
        n <- 0 to 9
        comb <- combinations
        tempDigits = basePrimeDigits.toBuffer
        if !(comb.contains(0) && n == 0)
        _ = { comb.foreach { tempDigits(_) = n } }
      } yield (comb, tempDigits.mkString.toLong)

      variants.groupBy(_._1).values.map(x => x.map(_._2))
    }
  }

  def problem50b = {
    val ceiling = 1000000L
    val primeCumSums = primes.takeWhile(_ < ceiling).scanLeft(0L)(_ + _)
    val maxLength = primeCumSums.indexWhere(_ > ceiling) - 1

    var result = 0L
    breakable {
      for {
        i <- maxLength to 1 by -1
        primeSums = (0 to primes.size - i).toStream.map(x => primeCumSums(i + x) - primeCumSums(x)).takeWhile(_ < ceiling)
        primeSum <- primeSums
        if Prime.isPrime(primeSum)
      } {
        result = primeSum
        break
      }
    }
    println(result)
  }

  def problem50 = {
    val ceiling = 1000000L
    val primeCumSums = primes.takeWhile(_ < ceiling).scanLeft(0L)(_ + _)
    var maxSumCount = 0
    val psc = new TruncatableStream(primeCumSums.zipWithIndex)
    val maximumPrimeSumsAndCountsForEachStartPrime = for {
      (startSum, startIndex) <- psc
      if primeCumSums(startIndex + maxSumCount) - startSum < ceiling || psc.truncate
      sumsWithCounts = Stream.from(startIndex + maxSumCount).map(endIndex => (primeCumSums(endIndex) - startSum, endIndex - startIndex))
      primeSumsBelowCeiling = sumsWithCounts.takeWhile({ case (sum, count) => sum < ceiling }).filter({ case (sum, count) => Prime.isPrime(sum) })
      if !primeSumsBelowCeiling.isEmpty
      (maxPrimeSum, maxPrimeSumCount) = primeSumsBelowCeiling.maxBy({ case (sum, count) => count })
      _ = if (maxSumCount < maxPrimeSumCount) maxSumCount = maxPrimeSumCount
    } yield (maxPrimeSum, maxPrimeSumCount)

    println(maximumPrimeSumsAndCountsForEachStartPrime.maxBy({ case (sum, count) => count }))
  }

  def problem49() = {
    val fourdigitprimes = primes.dropWhile(_ <= 1000).takeWhile(_ < 9999)

    val res = for {
      p <- fourdigitprimes
      p2 <- fourdigitprimes.dropWhile(_ <= p)
      p3 = 2 * p2 - p
      if Prime.isPrime(p3) & ispermutation(p, p2, p3)
    } yield (p, p2, p3)

    println(res.toList)

  }

  def problem48() = {
    val bigsum = (1 to 1000).map(x => (1 to x).foldLeft(BigInt(1))((sum, y) => sum * x)).foldLeft(BigInt(0))(_ + _)
    println(bigsum.toString.takeRight(10))
  }

  def problem47() = {
    println(for {
      n <- Stream.from(1)
      if (0 to 3).forall(x => primeFactors(n + x).groupBy(x => x).values.size == 4)
    } yield n)
  }

  def problem46() = {
    val compositeOdds = Stream.from(4).map(2 * _ + 1).filter(x => !Prime.isPrime(x))
    val res = for {
      n <- compositeOdds
      if Stream.from(1).map(x => 2 * x * x).takeWhile(_ < n - 1).forall(x => !Prime.isPrime(n - x))
    } yield n
    println(res.head)
  }

  def problem45 = {
    val ts = Stream.from(286).map(x => triangle(x.toLong))
    val res = for {
      t <- ts
      np = npentagon(t)
      if (np == np.toLong)
      nh = nhexagon(t)
      if (nh == nh.toLong)
    } yield (ntriangle(t).toLong, np.toLong, nh.toLong, t)
    println(res.head)
  }

  def problem44() = {
    //def pendiff(n:Long) = 3 * n + 1

    val pents = pentstream(1)

    def imax(pd: Long): Int = {
      floor((-5.0 + sqrt(25 + 24 * pd)) / 6).toInt
    }

    val res = for {
      pdiff <- pents
      i <- 1 to imax(pdiff)
      nlowerfrac = (2.0 * pdiff - i * (3 * i - 1)) / (6 * i)
      if nlowerfrac % 1 == 0
      nlower = nlowerfrac.toLong
      nupper = nlower.toLong + i
      plower = pentagon(nlower)
      pupper = pentagon(nupper)
      if isPentagon(plower + pupper)
    } yield (nlower, nupper, plower, pupper, i)

    println(res.head)
    /*
  val res = for {(nd, pd) <- ps
       n <- 1 to (pd.toInt - 1)/3
       m = sqrt( 36 * n * n - 12 * n + 1 + 24 * pd ) 
       if (m + 1) % 6 == 0
       (pn, pndash) = (pentagon(n), pentagon((m.toInt + 1) / 6))
       if (isPentagon(pn + pndash))
  } yield (pn, pndash)
  println(res.take(1).toList)
  println("end" + (((new Date).getTime - start.getTime))/1000)
  */

    /*
  for {(ndiff, diff) <- ps
       _ = println(ndiff,diff)
       (nfirst, first) <- ps.takeWhile(e => pendiff(e._1) <= diff)
       (nsecond, second) <- ps.drop(nfirst.toInt).takeWhile(e => (e._2 - first) <= diff)
       if (second - first) == diff && isPentagon(second + first)
  } yield (nfirst, nsecond, first, second, ndiff, diff)
  */

    /*
  (1 to 100).foreach(x => println(x, pentagon(x), isPentagon(pentagon(x))))
  */
    /*
  val pairs = for {i <- Stream.from(2)
                   j <- 1 to i - 1} yield (i, j)
  
  val res = pairs.map({case (a, b) => ((a, pentagon(a)), (b, pentagon(b)))}).
    filter({case ((a, pa), (b, pb)) => isPentagon(pa + pb) && isPentagon(pa - pb)})
    
  res.map({case ((a, pa), (b, pb)) => (a, b, pa, pb, pa + pb, pa - pb, pentagon(a) - pentagon(a - 1)) }).foreach(println)
  */

  }

  def problem43() = {
    val perms = "0123456789".permutations.filter(_(0) != '0').toStream

    def sub3Int(n: Int, s: String) = {
      s.substring(n - 1, n + 2).toInt
    }

    val filtered = perms.filter(s => {
      sub3Int(2, s) % 2 == 0 &&
        sub3Int(3, s) % 3 == 0 &&
        sub3Int(4, s) % 5 == 0 &&
        sub3Int(5, s) % 7 == 0 &&
        sub3Int(6, s) % 11 == 0 &&
        sub3Int(7, s) % 13 == 0 &&
        sub3Int(8, s) % 17 == 0
    })

    print(filtered.toList.map(_.toLong).sum)

  }

  def problem407_inprogress() = {
    def M(n: Int): Int = {
      val iList = (for (i <- (n / 2) to n - 1; if i * i % n == i) yield i)
      val res = if (iList.isEmpty) 1 else iList.max
      if (n % 10000 == 0) println(n, res, (res * res) % n)
      res
    }

    (1 to 100).foreach(x => println(x, M(x)))
    val res = Stream.from(2).takeWhile(_ <= 10000000).map(M _).foldLeft(0)(_ + _)
    println(res)
  }

  def problem42() = {
    val source = Source.fromFile("resources/words.txt")
    val words = source.mkString
    source.close()
    val QuotedWord = """"(\w+)"""".r
    val wordsList = words.split(",").map({ case QuotedWord(w) => w })
    println(wordsList.filter(w => isTriangular(wordValue(w))).size)

    def wordValue(w: String): Long = w.map(_ - 'A' + 1).sum
    def isTriangular(n: Long) = isSquare(n * 8 + 1)
  }

  def problem41() = {
    val index = primes.takeWhile(_ <= 7654321).lastIndexWhere(x => (x.toString diff "1234567").isEmpty)
    println((index, Prime(index + 1)))
  }

  def problem40() = {
    def digitCoord(pos: Long) = {
      def iter(pos: Long, count: Int): (Int, Long) = {
        val sectionSize = intpow(10, count - 1) * 9 * count
        if (sectionSize < pos) iter(pos - sectionSize, count + 1)
        else (count, pos)
      }
      iter(pos, 1)
    }

    println((0 to 6).map(x => {
      val pos = intpow(10L, x);
      val (section, remainder) = digitCoord(pos)
      //println(section, remainder)
      findDigit(section, remainder)
    }).foldLeft(1)(_ * _))

    def findDigit(section: Int, n: Long): Int = {
      if (section == 1) n.toInt
      else {
        val nthnum = (n + section - 1) / section
        val number = intpow(10, section - 1) + nthnum - 1
        val numberdigits = digits(number)
        numberdigits((n - 1).toInt % section)
      }
    }
  }

  def problem39() {
    val triangles = for {
      a <- 1 to 500
      b <- a to 1000
      c <- b + 1 to 1000
      p = a + b + c
      if p <= 1000
      if a * a + b * b == c * c
    } yield (p, (a, b, c))

    val result = triangles.
      groupBy({ case (p, (a, b, c)) => p }).
      map({ case (p, list) => (p, list.size) }).
      foldLeft((0, 0))((x: (Int, Int), e: (Int, Int)) =>
        if (e._2 > x._2) e else x)
    println(result)
  }

  def problem38() = {
    def oneToNine = 1 to 9

    def panDigitalMultOrZero(n: Long): Long = {
      def iter(dgts: List[Int], n: Long, mult: Int): List[Int] = {
        val newdgts = dgts ++ digits(n * mult)
        if (newdgts.size > 9) List(0)
        else if (removeAllFrom(newdgts, oneToNine).isEmpty)
          if (newdgts.size == 9) newdgts else iter(newdgts, n, mult + 1)
        else List(0)
      }
      iter(List(), n, 1).mkString.toLong
    }

    val res = (1 to 500000).toStream.map(panDigitalMultOrZero(_)).foldLeft(0L)((x, y) => if (y > x) y else x)
    println(res)

  }

  def problem37() {
    var count = 0;
    println(primes.dropWhile(_ <= 10).filter(isTruncatablePrime).take(11).sum)

    def isTruncatablePrime(n: Long): Boolean = {
      def left(d: Seq[Int]): Boolean =
        if (d.tail.isEmpty) true
        else if (!Prime.isPrime(d.tail.mkString.toLong)) false
        else left(d.tail)

      def right(d: Seq[Int]): Boolean =
        if (d.init.isEmpty) true
        else if (!Prime.isPrime(d.init.mkString.toLong)) false
        else right(d.init)

      val dgts = digits(n)
      left(dgts) && right(dgts)
    }
  }

  def problem36() = {
    println((1 until 1000000).filter(isPalindromic _).sum)

    def isPalindromic(n: Int) = {
      def isPal(s: String) = s == s.reverse
      isPal(Integer.toBinaryString(n)) && isPal(n.toString)
    }
  }

  def problem35() = {
    println(Prime.streamUpTo(1000000).filter(isCircular).length)

    def isCircular(n: Long): Boolean = {
      def isCircular0(digits: Seq[Int]): Boolean = {
        val rotDigits = rotateDigits(digits)
        val rotNum = rotDigits.mkString.toLong
        if (rotNum == n) true
        else if (!Prime.isPrime(rotNum)) false
        else isCircular0(rotDigits)
      }

      def rotateDigits(digits: Seq[Int]) = digits.tail :+ digits.head

      isCircular0(n.toString.map(_.asDigit))
    }
  }
  def problem34() {
    val curiousNums = for {
      n <- 10L to 2540160L
      digits = n.toString.map(_.asDigit.toLong)
      if (digits.map(fact).sum == n)
    } yield n

    println(curiousNums.sum)
  }

  def problem33() {
    def digits(n: Int): List[Int] = n.toString.map(_.asDigit).toList

    def cancels(a: Int, b: Int): Boolean =
      digits(a).intersect(digits(b)).exists(common => {
        val adigit = (digits(a) diff List(common)).head
        val bdigit = (digits(b) diff List(common)).head
        reduceFrac((adigit, bdigit)) == reduceFrac((a, b))
      })

    def reduceFrac(frac: (Int, Int)): (Int, Int) = frac match {
      case (a, b) => { val g = gcf(a, b); ((a / g).toInt, (b / g).toInt) }
    }

    val ns = for (i <- 1 to 9; j <- 1 to 9) yield i * 10 + j
    val pairs = for (
      num <- ns; den <- ns;
      if (num < den && cancels(num, den))
    ) yield (num, den)

    println(reduceFrac(pairs.fold(1, 1)({ case ((a1, b1), (a2, b2)) => (a1 * a2, b1 * b2) })))
  }
  def problem32() = {
    val digits = (1 to 9).toList

    val result =
      for {
        n1 <- 1 to 4
        comb1 <- digits.combinations(n1)
        n2 <- 1 to (5 - n1)
        comb2 <- (digits -- (comb1)).combinations(n2)
        comb3 = (digits -- (comb1 ++ comb2))
        perm1 <- comb1.permutations.map(_.mkString.toInt)
        perm2 <- comb2.permutations.map(_.mkString.toInt)
        perm3 <- comb3.permutations.map(_.mkString.toInt)
        if (perm1 * perm2 == perm3)
      } yield (perm1, perm2, perm3)

    println(result.map({ case (a, b, c) => c }).toSet.sum)
  }

  def problem31() = {
    def countChange(n: Int, coins: List[Int]): Int =
      if (n == 0) 1 else if (n < 0 || coins.isEmpty) 0
      else countChange(n - coins.head, coins) + countChange(n, coins.tail)

    val allCoins = List(1, 2, 5, 10, 20, 50, 100, 200)

    println(countChange(200, allCoins))
  }

  def problem30() {
    println((2 to 500000).filter(isSumOf5thPowerOfDigits).sum)

    def isSumOf5thPowerOfDigits(n: Int) = {
      n.toString.map(c => BigInt(c.asDigit).pow(5)).sum == n
    }
  }

  def problem29() = {
    val result = (for (a <- ((2 to 100).toSet[Int]); b <- 2 to 100)
      yield BigInt(a).pow(b)).size
    println(result)
  }

  def problem28() {
    println((1 to 501).map(x => 16 * x * x - 28 * x + 16).sum - 3)
  }

  def problem27() = {
    def quadratic(a: Int, b: Int): Long => Long = n => n * n + a * n + b

    def countPrimes(quad: Long => Long): Int = {
      var count = -1;
      Stream.from(0).find(n => { count += 1; !Prime.isPrime(quad(n)) })
      count
    }

    val primeCounts = for (a <- -999 to 999; b <- -999 to 999)
      yield (a, b, countPrimes(quadratic(a, b)))

    val result = primeCounts.maxBy({ case (a, b, count) => count }) match {
      case (a, b, count) => a * b
    }
    println(result)
  }

  def problem26() = {
    def reciprocalDigits(n: Long): Stream[(Long, Long)] = {
      def digits(num: Long, dividand: Long): Stream[(Long, Long)] = {
        (dividand / num, dividand % num) #:: digits(num, (dividand % num) * 10)
      }
      digits(n, 10)
    }

    def findCycleLength(s: Stream[Any]): Long = {
      var i = -1L;
      s.find(x => { i += 1; s.indexOf(x) < i })
      i
    }

    println((1 until 1000).map(x => (x, findCycleLength(reciprocalDigits(x)))).maxBy({ case (n, m) => m })._1)
  }

  def problem25() {
    var count = 0;
    fib.find(x => { count += 1; x >= BigInt(10).pow(999) })
    println(count - 1)
  }

  def problem24() = {
    val startPerm = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
    val allPerms = startPerm.permutations.map(_.mkString).toList.sorted
    println(allPerms(999999))
  }

  def problem23() = {
    def isAbundant(n: Int) = properDivisors(n).sum > n

    val abundantNumbers = (1 to 28123).filter(isAbundant)

    def canSumFrom(n: Int, lst: Seq[Int]) = {
      val testList = lst.takeWhile(_ < n).toSet
      testList.exists(l1 => testList.contains(n - l1))
    }

    println((1 to 28123).filter(!canSumFrom(_, abundantNumbers)).sum)
  }

  def problem22() = {
    val source = Source.fromFile("resources/names.txt")
    val names = source.mkString
    source.close()
    val namesList = names.split("[,\"]+")

    def nameValue(name: String): Int = {
      name.map(_ - 'A' + 1).sum
    }

    val result = namesList.sorted.map(nameValue).foldLeft((1, 0))((pair, value) => {
      pair match {
        case (pos, tot) => (pos + 1, tot + value * pos)
      }
    })

    println(result._2)
  }

  def problem21() = {

    def isAmicable(n: Long) = {
      val sumdiv = Factor.properDivisors(n).sum
      sumdiv != n && Factor.properDivisors(sumdiv).sum == n
    }

    println((2L to 10000L).filter(isAmicable).sum)
  }

  def problem20() {
    println((2 to 100).foldLeft(BigInt(1))(_ * _).toString.map(_.asDigit).sum)
  }

  def problem19() = {
    val monthdays = List(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    def isLeapYear(yr: Int) = yr % 4 == 0 && (yr % 100 != 0 || yr % 400 == 0)

    def daysInMonth(month: Int, year: Int) = {
      if (month == 2 && isLeapYear(year)) 29 else monthdays(month)
    }

    def addWeek(dmy: (Int, Int, Int)): (Int, Int, Int) = {
      val (day, month, year) = dmy
      val mdays = daysInMonth(month, year)
      if (day + 7 <= mdays) (day + 7, month, year)
      else if (month < 12) ((day + 7) % mdays, month + 1, year)
      else ((day + 7) % mdays, 1, year + 1)
    }

    def countFirstSundaysAfter(dmy: (Int, Int, Int)): Long = {
      val (day, month, year) = addWeek(dmy)
      if (year == 2001) 0
      else countFirstSundaysAfter((day, month, year)) + (if (day == 1 && year > 1900) 1 else 0)
    }

    println(countFirstSundaysAfter((7, 1, 1900)))
  }

  def problem18() = {
    val input = """75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""

    val tridata = input.split("\n").map(_.split(" ").map(_.toInt).toList).toList

    def maxpath(row: Int, col: Int): Long = {
      if (row < 0 || row >= tridata.size || col < 0 || col >= tridata(row).size) 0L
      else tridata(row)(col) + max(maxpath(row + 1, col), maxpath(row + 1, col + 1))
    }

    println(maxpath(0, 0))
  }

  def problem17() {
    val oneToNineteen = List(
      "",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine",
      "ten",
      "eleven",
      "twelve",
      "thirteen",
      "fourteen",
      "fifteen",
      "sixteen",
      "seventeen",
      "eighteen",
      "nineteen")

    val tens = List("", "ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety")
    val hundred = "hundred"
    val thousand = "thousand"

    def sayNumber(n: Int): String = {
      (if (n / 1000 > 0) oneToNineteen(n / 1000) + thousand else "") +
        (if ((n % 1000) / 100 > 0) oneToNineteen((n % 1000) / 100) + hundred else "") +
        (if (n % 100 > 0) checkAnd(n) + sayOneToNinetyNine(n % 100) else "")
    }

    def sayOneToNinetyNine(n: Int): String = {
      if (n < 20) oneToNineteen(n)
      else tens(n / 10) + oneToNineteen(n % 10)
    }

    def checkAnd(n: Int): String = {
      if (n > 100 && n % 100 > 0) "and" else ""
    }

    println((1 to 1000).map(sayNumber(_)).foldLeft(0)(_ + _.length()))
  }

  def problem16() {
    println(BigInt(2).<<(999).toString.map(_.asDigit).reduce(_ + _))
  }

  def problem15() = {
    type PathCache = Map[(Int, Int), Long]
    def countpaths(gridsize: Int): Long = {
      def countpaths0(x: Int, y: Int, size: Int, cache: PathCache): (Long, PathCache) = {
        if (x > size || y > size) (0L, cache)
        else if (x == size && y == size) (1L, cache)
        else if (cache.contains((x, y))) (cache((x, y)), cache)
        else {
          val (nright, cache2) = countpaths0(x + 1, y, size, cache)
          val (ndown, cache3) = countpaths0(x, y + 1, size, cache2)
          val result = nright + ndown
          (result, cache3.updated((x, y), result))
        }
      }
      val cache = Map[(Int, Int), Long]()
      countpaths0(0, 0, gridsize, cache) match { case (npaths, _) => npaths }
    }

    println(countpaths(20))
  }

  def problem14() = {
    def collatz(n: Long): Stream[Long] = {
      if (n == 1) 1L #:: Stream.empty
      else if (n % 2 == 0) n #:: collatz(n / 2)
      else n #:: collatz(3 * n + 1)
    }

    val result = (1 to 1000000).foldLeft((0, 0))((prevmax, i) => {
      val len = collatz(i).size
      prevmax match {
        case (prevn, prevlen) =>
          if (len > prevlen) (i, len) else prevmax
      }
    })

    println(result)
  }

  def problem13() = {
    val input = """37107287533902102798797998220837590246510135740250
46376937677490009712648124896970078050417018260538
74324986199524741059474233309513058123726617309629
91942213363574161572522430563301811072406154908250
23067588207539346171171980310421047513778063246676
89261670696623633820136378418383684178734361726757
28112879812849979408065481931592621691275889832738
44274228917432520321923589422876796487670272189318
47451445736001306439091167216856844588711603153276
70386486105843025439939619828917593665686757934951
62176457141856560629502157223196586755079324193331
64906352462741904929101432445813822663347944758178
92575867718337217661963751590579239728245598838407
58203565325359399008402633568948830189458628227828
80181199384826282014278194139940567587151170094390
35398664372827112653829987240784473053190104293586
86515506006295864861532075273371959191420517255829
71693888707715466499115593487603532921714970056938
54370070576826684624621495650076471787294438377604
53282654108756828443191190634694037855217779295145
36123272525000296071075082563815656710885258350721
45876576172410976447339110607218265236877223636045
17423706905851860660448207621209813287860733969412
81142660418086830619328460811191061556940512689692
51934325451728388641918047049293215058642563049483
62467221648435076201727918039944693004732956340691
15732444386908125794514089057706229429197107928209
55037687525678773091862540744969844508330393682126
18336384825330154686196124348767681297534375946515
80386287592878490201521685554828717201219257766954
78182833757993103614740356856449095527097864797581
16726320100436897842553539920931837441497806860984
48403098129077791799088218795327364475675590848030
87086987551392711854517078544161852424320693150332
59959406895756536782107074926966537676326235447210
69793950679652694742597709739166693763042633987085
41052684708299085211399427365734116182760315001271
65378607361501080857009149939512557028198746004375
35829035317434717326932123578154982629742552737307
94953759765105305946966067683156574377167401875275
88902802571733229619176668713819931811048770190271
25267680276078003013678680992525463401061632866526
36270218540497705585629946580636237993140746255962
24074486908231174977792365466257246923322810917141
91430288197103288597806669760892938638285025333403
34413065578016127815921815005561868836468420090470
23053081172816430487623791969842487255036638784583
11487696932154902810424020138335124462181441773470
63783299490636259666498587618221225225512486764533
67720186971698544312419572409913959008952310058822
95548255300263520781532296796249481641953868218774
76085327132285723110424803456124867697064507995236
37774242535411291684276865538926205024910326572967
23701913275725675285653248258265463092207058596522
29798860272258331913126375147341994889534765745501
18495701454879288984856827726077713721403798879715
38298203783031473527721580348144513491373226651381
34829543829199918180278916522431027392251122869539
40957953066405232632538044100059654939159879593635
29746152185502371307642255121183693803580388584903
41698116222072977186158236678424689157993532961922
62467957194401269043877107275048102390895523597457
23189706772547915061505504953922979530901129967519
86188088225875314529584099251203829009407770775672
11306739708304724483816533873502340845647058077308
82959174767140363198008187129011875491310547126581
97623331044818386269515456334926366572897563400500
42846280183517070527831839425882145521227251250327
55121603546981200581762165212827652751691296897789
32238195734329339946437501907836945765883352399886
75506164965184775180738168837861091527357929701337
62177842752192623401942399639168044983993173312731
32924185707147349566916674687634660915035914677504
99518671430235219628894890102423325116913619626622
73267460800591547471830798392868535206946944540724
76841822524674417161514036427982273348055556214818
97142617910342598647204516893989422179826088076852
87783646182799346313767754307809363333018982642090
10848802521674670883215120185883543223812876952786
71329612474782464538636993009049310363619763878039
62184073572399794223406235393808339651327408011116
66627891981488087797941876876144230030984490851411
60661826293682836764744779239180335110989069790714
85786944089552990653640447425576083659976645795096
66024396409905389607120198219976047599490197230297
64913982680032973156037120041377903785566085089252
16730939319872750275468906903707539413042652315011
94809377245048795150954100921645863754710598436791
78639167021187492431995700641917969777599028300699
15368713711936614952811305876380278410754449733078
40789923115535562561142322423255033685442488917353
44889911501440648020369068063960672322193204149535
41503128880339536053299340368006977710650566631954
81234880673210146739058568557934581403627822703280
82616570773948327592232845941706525094512325230608
22918802058777319719839450180888072429661980811197
77158542502016545090413245809786882778948721859617
72107838435069186155435662884062257473692284509516
20849603980134001723930671666823555245252804609722
53503534226472524250874054075591789781264330331690"""

    val result = input.split("""\s""").map(BigInt(_)).reduceLeft(_ + _)
    println(result.toString substring (0, 10))
  }

  def problem12() {
    def ndivisors(n: Long): Int = {
      val factors = Factor.primeFactors(n)
      factors.groupBy(x => x).map({ case (k, v) => v.size }).foldLeft(1)((x, y) => x * (y + 1))
    }

    val result = Stream.from(1).find(x => ndivisors(triangle(x)) > 500)

    println(result match { case Some(res) => triangle(res) })
  }

  def problem11() {
    val input =
      "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 " +
        "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 " +
        "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 " +
        "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 " +
        "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 " +
        "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 " +
        "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 " +
        "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 " +
        "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 " +
        "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 " +
        "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 " +
        "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 " +
        "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 " +
        "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 " +
        "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 " +
        "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 " +
        "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 " +
        "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 " +
        "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 " +
        "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48";

    var grid = (input.split(" ").map(_.toInt) grouped 20).toList
    println(maxRow(grid, 0))

    def maxRow(grid: Seq[Array[Int]], row: Int): Long = {
      if (row >= grid.size) 0 else max(maxCol(grid, row, 0), maxRow(grid, row + 1))
    }

    def maxCol(grid: Seq[Array[Int]], row: Int, col: Int): Long = {
      if (col >= grid.head.size) 0 else max(maxSum(grid, row, col), maxCol(grid, row, col + 1))
    }

    def maxSum(grid: Seq[Array[Int]], row: Int, col: Int): Long = {
      Seq(prod(1, 0, row, col, grid),
        prod(0, 1, row, col, grid),
        prod(1, 1, row, col, grid),
        prod(1, -1, row, col, grid)).max
    }

    def prod(dx: Int, dy: Int, row: Int, col: Int, grid: Seq[Array[Int]]): Long = {
      (0 until 4).foldLeft(1L)((current, offset) => { safegrid(grid, row + offset * dx, col + offset * dy) * current })
    }

    def safegrid(grid: Seq[Array[Int]], row: Int, col: Int): Int = {
      if (row >= grid.size - 3 || col < 0 || col >= grid.head.size) 0 else grid(row)(col)
    }
  }

  def problem10() {
    println(Prime.streamUpTo(2000000).sum)
  }

  def problem9() {

    val tuples = (for (x <- 1 to 1000; y <- 1 to 1000) yield (x, y, sqrt(square(x) + square(y)))) filter (_._3 % 1 == 0)
    val tuple = tuples.find(x => x._1 + x._2 + x._3 == 1000) match {
      case Some(x) => x
    }
    print(tuple, tuple._1 * tuple._2 * tuple._3.toLong)
  }

  def problem8() {
    val s = """73167176531330624919225119674426574742355349194934
				96983520312774506326239578318016984801869478851843
				85861560789112949495459501737958331952853208805511
				12540698747158523863050715693290963295227443043557
				66896648950445244523161731856403098711121722383113
				62229893423380308135336276614282806444486645238749
				30358907296290491560440772390713810515859307960866
				70172427121883998797908792274921901699720888093776
				65727333001053367881220235421809751254540594752243
				52584907711670556013604839586446706324415722155397
				53697817977846174064955149290862569321978468622482
				83972241375657056057490261407972968652414535100474
				82166370484403199890008895243450658541227588666881
				16427171479924442928230863465674813919123162824586
				17866458359124566529476545682848912883142607690042
				24219022671055626321111109370544217506941658960408
				07198403850962455444362981230987879927244284909188
				84580156166097919133875499200524063689912560717606
				05886116467109405077541002256983155200055935729725
				71636269561882670428252483600823257530420752963450""".filter(_.isDigit)

    var n = 0
    for (i <- 0 to s.length - 1) {
      val ss = s.substring(i, min(s.length, i + 5))
      n = max(n, ss.map(_.asDigit).reduceLeft(_ * _))
    }
    println(n)
  }

  def problem7() {
    println(primes(10000))
  }

  def problem6() {
    val sumsquare = (1 to 100).map(x => x * x).sum
    val sum = (1 to 100).sum
    println(sum * sum - sumsquare)
  }

  def problem5() {
    var n: Long = 1
    for (i <- 2 to 20 if n % i > 0)
      n *= i / gcf(n, i)
    println(n)
  }

  def problem4() {

    val palindromes = for (
      i <- Range(100, 999);
      j <- Range(100, 999) if isPalindrome(i * j)
    ) yield i * j
    println(palindromes.max)
  }

  def problem3() {
    val n: Long = 600851475143L
    for (i <- Prime.stream takeWhile (_ < sqrt(n)) if n % i == 0)
      println(i)
  }

  def problem2() {
    val v: BigInt = (for (i <- fib drop 2 takeWhile (_ < 4000000) if i % 2 == 0) yield i).sum
    println(v)
  }

  def problem1() {
    val v = (1 to 999).filter(x => x % 3 == 0 || x % 5 == 0).sum
    println(v)
  }
  */

}