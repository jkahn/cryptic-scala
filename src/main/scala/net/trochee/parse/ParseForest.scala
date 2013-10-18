package net.trochee.parse

class ParseForest(s: String) {
  abstract class Span {
    def start: Position
    def end: Position
    def likelihood: Probability
    def label: Label
    def spanString: String = "(" + start + "->" + end + ")"
    def likelihoodString: String = "[" + likelihood + "]"
  }
  class Terminal(val label: Label, val start: Position, val end: Position) extends Span {
    override def toString(): String = s.subSequence(start, end) + spanString
    val prob = 1.0
    def likelihood = prob
  }
  class NonTerminal(val label: Label, val spans: Seq[Span]) extends Span {
    override def toString(): String = label + "(" + spans + ") " + spanString
    def start: Position = spans.head.start
    def end: Position = spans(spans.length - 1).end
    val prob = 1.0
    lazy val likelihood = prob * spans.map(_.likelihood).foldLeft(prob)(_ * _)
  }
}
object ParseForest {
  def apply(s: String) = new ParseForest(s)
}
 
