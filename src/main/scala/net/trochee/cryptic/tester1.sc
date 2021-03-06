package net.trochee.cryptic

//import net.trochee.cryptic._
//import collection.immutable.HashSet
//import HashSet
object tester1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val cands: SolutionSet = SolutionSet.fromSolutions("WRENCH", "MONKEY", "HARDY", "HARDBALL")
                                                  //> cands  : net.trochee.cryptic.SolutionSet = SolutionSet(WRENCH, MONKEY, HARDY
                                                  //| , HARDBALL)
  //val cands: SolutionSet = new SolutionSet(solns)
  val lengthConstraint = HasLength(5)             //> lengthConstraint  : net.trochee.cryptic.HasLength = HasLength(5)

  cands.constrain(HasLength(8))                   //> res0: net.trochee.cryptic.SolutionSet = SolutionSet(HARDBALL)

  val x = HasLength(5).constrain(cands)           //> x  : net.trochee.cryptic.SolutionSet = SolutionSet(HARDY)

  //val constrained:SolutionSet = Constraint.constrain(cands, constraints)
  val constrained:SolutionSet = cands.constrain(HasLength(6) , CharAtPos('W',0))
                                                  //> constrained  : net.trochee.cryptic.SolutionSet = SolutionSet(WRENCH)


  val constraints: Seq[Constraint] = Seq(CharAtPos('W', 0), HasLength(6))
                                                  //> constraints  : Seq[net.trochee.cryptic.Constraint] = List(CharAtPos(W@0), Ha
                                                  //| sLength(6))
  val constrained2 = cands.constrain(constraints:_*)
                                                  //> constrained2  : net.trochee.cryptic.SolutionSet = SolutionSet(WRENCH)

	val anagramConstraint = Anagram("hen cwr")//> anagramConstraint  : net.trochee.cryptic.Anagram = Anagram(CEHNRW)
	
	val f = anagramConstraint.constraint("WRENCH")
                                                  //> f  : Boolean = true
  val g = Hidden("Fop era telly")                 //> g  : net.trochee.cryptic.Hidden = Hidden(FOPERATELLY)
                                                  
  val h = Anagram("E.T. opera")                   //> h  : net.trochee.cryptic.Anagram = Anagram(AEEOPRT)
  assert(g.constraint("OPERATE"))
  assert(h.constraint("OPERATE"))
  
  val solutions = SolutionSet.fromSolutions("OPERATE", "SKEEBALL", "APPLE", "FROYO")
                                                  //> solutions  : net.trochee.cryptic.SolutionSet = SolutionSet(OPERATE, SKEEBAL
                                                  //| L, APPLE, FROYO)
  println(solutions.constrain(g,h))               //> SolutionSet(OPERATE)
  val i = Composed(Hidden("Fop era telly"), Anagram("E.T. opera"))
                                                  //> i  : net.trochee.cryptic.Composed = Composed[Hidden(FOPERATELLY),Anagram(AE
                                                  //| EOPRT)]
  solutions.constrain(i)                          //> res1: net.trochee.cryptic.SolutionSet = SolutionSet(OPERATE)
  
 
    
}