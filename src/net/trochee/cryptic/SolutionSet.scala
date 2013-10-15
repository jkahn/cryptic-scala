package net.trochee.cryptic

class SolutionSet(val sset:Set[Solution]) {
	override def toString = "Solution" + sset.toString
	def constrain(constraint:Constraint) = constraint.constrain(this)
	// TODO: consider applying the constraints in order
	def constrain(constraints:Constraint*):SolutionSet = constraints.foldLeft[SolutionSet](this)((cs, c) => c.constrain(cs))
	
	//TODO: compressionRatio
}
object SolutionSet {
	def apply(sset:Set[Solution]) = new SolutionSet(sset)
	def fromSolutions(sseq:Solution*) = new SolutionSet(sseq.toSet)
}