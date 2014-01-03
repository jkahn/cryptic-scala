package net.trochee.parse

object tester2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val pf = ParseForest.fromTokens(Seq("hides", "opera"))
                                                  //> pf  : net.trochee.parse.ParseForest = ParseForest(List(Stream(hides[0.980], 
                                                  //| ?), Stream(opera[0.400], ?)))
                                               
  pf.topAnalysis                                  //> res0: Seq[net.trochee.parse.LabeledSpan] = List(hides[0.980], opera[0.400])
  pf.nextAnalyses.toList                          //> res1: List[net.trochee.parse.ParseForest] = List(ParseForest(List(Stream(hid
                                                  //| es[0.400], ?), Stream(opera[0.400]))))
  
  pf.spanningAnalyses.toList                      //> res2: <error> = List('Hidden(hides[0.980] opera[0.400]) [0.376], 'Content(hi
                                                  //| des[0.400] opera[0.400]) [0.128])
  
  val pf2 = ParseForest.fromTokens(Seq("E.T.", "opera"))
                                                  //> pf2  : net.trochee.parse.ParseForest = ParseForest(List(Stream(E.T.[0.400], 
                                                  //| ?), Stream(opera[0.400], ?)))
  pf2.spanningAnalyses.toList                     //> res3: <error> = List('Content(E.T.[0.400] opera[0.400]) [0.128])
                                                  
  val r = 0 until 9                               //> r  : scala.collection.immutable.Range = Range(0, 1, 2, 3, 4, 5, 6, 7, 8)
  r.sliding(2).toList                             //> res4: List[scala.collection.immutable.IndexedSeq[Int]] = List(Vector(0, 1), 
                                                  //| Vector(1, 2), Vector(2, 3), Vector(3, 4), Vector(4, 5), Vector(5, 6), Vector
                                                  //| (6, 7), Vector(7, 8))

  val seq = "ABCDEFGH".toSeq                      //> seq  : Seq[Char] = ABCDEFGH
  val start = 1                                   //> start  : Int = 1
  val end = 3                                     //> end  : Int = 3
  seq.slice(start, end)                           //> res5: Seq[Char] = BC
	seq.take(start)                           //> res6: Seq[Char] = A
	seq.takeRight(seq.length -end)            //> res7: Seq[Char] = DEFGH
	
	
}