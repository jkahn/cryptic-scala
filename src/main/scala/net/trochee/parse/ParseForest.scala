package net.trochee.parse

import scala.annotation.tailrec

abstract trait Weighted {
  def prob: Probability
  def likelihoodString: String = "[" + prob + "]"
}

abstract trait LabeledSpan extends Weighted {
  def label: Label
}

case class Terminal(val label: Label, val prob: Probability, val tok: Token) extends LabeledSpan {
  // prob = if (label == "W") 0.9 else 1.0
  lazy val likelihood = prob
  override def toString: String = tok + likelihoodString
}
case class TerminalType(val label: Label, val prob: Probability, val tokenfilter: (Token => Boolean))
  extends PartialFunction[Token, Terminal] with Weighted {
  def isDefinedAt(t: Token) = tokenfilter(t)
  def apply(t: Token) = Terminal(label, prob, t)
}

case class NTToken(val label: Label, val prob: Probability, val spans: Seq[LabeledSpan]) extends LabeledSpan {
  override def toString(): String = label + "(" + spans + ") " + likelihoodString
  lazy val likelihood = prob * spans.map(_.prob).foldLeft(prob)(_ * _)
}

case class NonTerminalType(val label: Label, val prob: Probability, val spans: Seq[Label])
  extends PartialFunction[Seq[LabeledSpan], NTToken] with Weighted {
  def isDefinedAt(s: Seq[LabeledSpan]) = s.map(_.label) == spans
  def apply(s: Seq[LabeledSpan]): NTToken = NTToken(label, prob, s)
}

class ParseForest(val tokens: Seq[Token]) {
  val terminals =
    Seq(TerminalType("HIDDEN", 0.98, (_.toLowerCase() == "hides")),
      TerminalType("ANAGRAM", 0.97, (_.toLowerCase() == "scrambles")),
      TerminalType("W", 0.4, (x => true)))

  val nonterminals =
    Seq(
      NonTerminalType("Anagram", 0.98, Seq("ANAGRAM", "OTHER")),
      NonTerminalType("Anagram", 0.97, Seq("OTHER", "ANAGRAM")),
      NonTerminalType("Hidden", 0.96, Seq("HIDDEN", "OTHER")),
      NonTerminalType("Hidden", 0.95, Seq("OTHER", "HIDDEN")),
      NonTerminalType("OTHER", 0.8, Seq("W", "W")),
      NonTerminalType("OTHER", 0.7, Seq("W", "OTHER")))

  /** returns probability-descending parses using spans given */
  def parsesAt(split: Position): SpanQueue = {
    val (l, r) = tokens.splitAt(split)

    val parses: SpanQueue = for {
      lParse <- ParseForest(l).spans;
      rParse <- ParseForest(r).spans;
      nt <- nonterminals;
      if (nt.isDefinedAt(Seq(lParse, rParse)))
    } yield nt(Seq(lParse, rParse))
    parses.sortBy(-_.prob)
    // TODO: think about how to return stream of highest-quality first without having to look at all available
  }

  // combinatorial exploration, best-first


  /**
   *  the spans possible for this token-sequence
   */
  lazy val spans: SpanQueue =
    tokens match {
      case Nil => Stream.Empty
      case Seq(t: Token) => (for { term <- terminals; if term.isDefinedAt(t) } yield term(t)).toStream
      case _ => ParseForest.merge(for {splitPn <- 0 to tokens.length} yield parsesAt(splitPn))
    }

}
object ParseForest {
  def apply(toks: Seq[String]) = new ParseForest(toks)

  //TODO: rewrite to use partially-sorted substreams

  def merge(input: Seq[SpanQueue]): SpanQueue =
    input.filterNot(_.isEmpty).sortBy(-_.head.prob) match {
      case Nil => Stream.Empty
      case head :: Nil => head // only one stream remaining, use it directly
      case topQ :: tail // return the first item from the first list, then re-sort
      => topQ.head #:: merge(topQ.tail :: tail)
    }
}
 
/*
 * parse forest stream algorithm
 * 
 * assume best-first immutable stream for each span
 *
 * compose whole span + subspans+composition
 * 
 * return as stream, best-first 
 */