package net.trochee.parse

abstract trait Weighted {
  def prob: Probability
  def likelihoodString: String = "[" + prob + "]"
}

abstract trait LabeledSpan extends Weighted {
  def label: Label
}

case class NTToken(val label: Label, val prob: Probability, val spans: Seq[LabeledSpan]) extends LabeledSpan {
  override def toString(): String = label + "(" + spans + ") " + likelihoodString
  lazy val likelihood = prob * spans.map(_.prob).foldLeft(prob)(_ * _)
}

case class NonTerminalType(label: Label, prob: Probability, spans: Seq[Label])
  extends  PartialFunction[Seq[LabeledSpan], NTToken] with Weighted {
  def isDefinedAt(x: Seq[LabeledSpan]): Boolean = x.map(_.label) == spans
  def apply(x: Seq[LabeledSpan]): NTToken = NTToken(label, prob, x)
  }

case class Terminal(val label: Label, val prob: Probability, val tok: Token) extends LabeledSpan {
  lazy val likelihood = prob
  override def toString: String = tok + likelihoodString
}
case class TerminalType(val label: Label, val prob: Probability, val tokenfilter: (Token => Boolean))
  extends PartialFunction[Token,Terminal] with Weighted {
  def isDefinedAt(t: Token):Boolean = tokenfilter(t)
  def apply(t: Token):Terminal = Terminal(label, prob, t)
  }


case class ParseForest(val aQueues: Seq[AnalysisQueue]) {

  val nonterminals =
    Seq(
      NonTerminalType('Anagram, 0.98, Seq('AnagramHint, 'Content)),
      NonTerminalType('Anagram, 0.97, Seq('Content, 'AnagramHint)),
      NonTerminalType('Hidden, 0.96, Seq('HiddenHint, 'Content)),
      NonTerminalType('Hidden, 0.95, Seq('Content, 'HiddenHint)),
      NonTerminalType('Content, 0.8, Seq('Content, 'Content)))

  /**
   * spanningAnalyses are all those single nonterminals that cover this entire region
   */
  lazy val spanningAnalyses: AnalysisQueue = 
    ParseForest.zipQueues(Seq(spanningNTAnalyses) ++ nextAnalyses.map(_.spanningAnalyses))

  val topAnalysis:Seq[LabeledSpan] = aQueues.map(_.head)

  /** 
   *  nonTerminalAnalyses - a queue of NT spans that allow an NT over the entire topAnalysis
   */
  lazy val spanningNTAnalyses: AnalysisQueue =
      (for { nt <- nonterminals.toStream; if nt.isDefinedAt(topAnalysis) }
        yield nt(topAnalysis))
  
  /** 
   *  all adjacent parse forests without applying any nonterminal rules
   *  
   *  choose the next one for each analysis bucket.
   */
  lazy val nextAnalyses: Seq[ParseForest] =
    for { aqIdx <- aQueues.indices;
    	  keyQ = aQueues(aqIdx);
    	  if keyQ.tail != Stream.Empty;
    	  prefix = aQueues.take(aqIdx)
    	  suffix = aQueues.takeRight(aQueues.length - aqIdx -1)
    } yield ParseForest(prefix ++ Seq(keyQ.tail) ++ suffix)

}
object ParseForest {
  val terminals =
    Seq(TerminalType('HiddenHint, 0.98, (_.toLowerCase() == "hides")),
      TerminalType('AnagramHint, 0.97, (_.toLowerCase() == "scrambles")),
      TerminalType('Content, 0.4, (x => true)))

  //TODO rewrite as collect call?
  def tokenstream(tok:Token): AnalysisQueue =
    (for {t <- terminals.toStream if t.isDefinedAt(tok)} yield t(tok))
 
  def fromTokens(toks: Seq[String]) = ParseForest(toks.map(tokenstream))

  /**
   * given an input sequence of priority queues, lazily report the next best 
   * one, heap-style
   */
  def zipQueues(input: Seq[AnalysisQueue]): AnalysisQueue = {
    val active = input.filterNot(_.isEmpty)
    val maxIdx = active.indices.maxBy(active(_).head.prob)
    val nextQueue = active.updated(maxIdx, active(maxIdx).tail)
    active(maxIdx).head #:: zipQueues(nextQueue)
  }

}