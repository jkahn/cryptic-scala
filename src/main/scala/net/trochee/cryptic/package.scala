package net.trochee

package object cryptic {
  type Clue = String
  type Solution = Seq[Char]
  //type SolutionSet = Set[Solution]
  def clean(chars:Clue): Solution = for (c<- chars; if c.isLetter) yield c.toUpper
  def clean(seq:Solution): Solution = clean(String.valueOf(seq.toArray))

}