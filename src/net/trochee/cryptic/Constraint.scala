package net.trochee.cryptic

abstract class Constraint {
	val constraint:(Solution => Boolean)
	def constrain(ss:SolutionSet): SolutionSet = SolutionSet(ss.sset.filter(constraint)) 
}

object Constraint {
//  def constrain(ss: SolutionSet, constraints:Seq[Constraint]):SolutionSet =
//    constraints.foldLeft(ss)((cs, c) => c.constrain(cs))
}

class HasLength(i:Integer) extends Constraint {
    val constraint:(Solution => Boolean) = (_.length == i)
    override def toString() = "HasLength(" + i + ")"
}
object HasLength {
	def apply(i: Integer) = new HasLength(i)
}

class CharAtPos(c:Char, p:Integer) extends Constraint {
	override def toString() = "CharAtPos(" + c + '@' + p + ')'
    val constraint:(Solution => Boolean) = (_(p) == c)
}
object CharAtPos {
  def apply(c:Char, pos:Integer) = new CharAtPos(c, pos)
}