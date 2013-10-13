package net.trochee.cryptic

abstract class ClueSolver(val clue:Clue) {
	def constraint:Constraint
}

class AnagramSolver (override val clue: Clue) extends ClueSolver(clue) {
	val constraint = Anagram(clue)
}