package net.trochee.parse

abstract class Span {
  def likelihood: Probability
  def label: Label
  def likelihoodString: String = "[" + likelihood + "]"

}
class ParseForest(val tokens: Seq[Token]) {

  class Terminal(val label: Label, val tok: Token) extends Span {
    val prob = 0.9
    lazy val likelihood = prob
    override def toString: String = tok + likelihoodString
  }
  object Terminal {
    def apply(label: Label, tok: Token) = new Terminal(label, tok)
    def default(t: Token) = Terminal("W", t)
    def acceptable(t: Token): Stream[Span] = {
      t match {
        case "hides" => Stream(Terminal("HIDDEN", t), default(t))
        case "scrambles" => Stream(Terminal("ANAGRAM", t), default(t))
        case _ => Stream(default(t))
      }
    }
  }

  class NonTerminal(val label: Label, val spans: Seq[Span]) extends Span {
    override def toString(): String = label + "(" + spans + ") " + likelihoodString
    val prob = 1.0
    lazy val likelihood = prob * spans.map(_.likelihood).foldLeft(prob)(_ * _)
  }
  object NonTerminal {
    def apply(label: Label, spans: Seq[Span]) = new NonTerminal(label, spans)
  }

  def subtrees(split: Position): (Stream[Span], Stream[Span]) = {
    val (l, r) = tokens.splitAt(split)
    return (ParseForest(l).spans, ParseForest(r).spans)
    //think about how to return stream of quality
  }

  lazy val nonTerminalSpans: Stream[Span] = {
    val splitPositions = 0 to tokens.length
    val spanPairs:Seq[(Stream[Span],Stream[Span])] = splitPositions.map(subtrees(_))

    ???
  }
  /**
   *  the spans possible for this token-sequence
   */
  lazy val spans: Stream[Span] =
    tokens match {
      case Nil => Stream.Empty
      case Seq(t: Token) => Terminal.acceptable(t)
      case _ => nonTerminalSpans
    }
  //def spans: Map[(Position,Position),Span]

}
object ParseForest {
  def apply(toks: Seq[String]) = new ParseForest(toks)
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