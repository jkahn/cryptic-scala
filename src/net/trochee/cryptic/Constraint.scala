package net.trochee.cryptic

abstract class Constraint {
	val constraint:(Solution => Boolean)
	def constrain(ss:SolutionSet): SolutionSet = SolutionSet(ss.sset.filter(constraint)) 
}

object Constraint {
//  def constrain(ss: SolutionSet, constraints:Seq[Constraint]):SolutionSet =
//    constraints.foldLeft(ss)((cs, c) => c.constrain(cs))
}

class HasLength(val len:Integer) extends Constraint {
    val constraint:(Solution => Boolean) = (_.length == len)
    override def toString() = "HasLength(" + len + ")"
}
object HasLength {
	def apply(i: Integer) = new HasLength(i)
}

class CharAtPos(val ch:Char, val pos:Integer) extends Constraint {
	override def toString() = "CharAtPos(" + ch + '@' + pos + ')'
    val constraint:(Solution => Boolean) = (_(pos) == ch)
}
object CharAtPos {
  def apply(c:Char, pos:Integer) = new CharAtPos(c, pos)
}

class Anagram (val in:Clue) extends Constraint {
  //val normalized = in.map(_.toUpper).sorted
  val normalized = Anagram.norm(in)
  override def toString() = "Anagram(" + normalized + ")"
  val constraint:(Solution => Boolean) = (s => (Anagram.norm(s) == normalized))
}
object Anagram {
  def apply (in:Clue) = new Anagram(in)
  def norm(chars:Solution):Seq[Char] =  (for (c <- chars; if c.isLetter) yield c.toUpper).sorted
}

class Hidden(c:Clue) extends Constraint {
  val normalized = Hidden.norm(c)
  override def toString() = "Hidden(" + normalized + ")"
  val constraint = (s:Solution) => normalized.containsSlice(Hidden.norm(s))
}
object Hidden {
  def apply(in: Clue) = new Hidden(in)
  def norm(chars:Solution):Seq[Char] = (for (c<- chars; if c.isLetter) yield c.toUpper)
}