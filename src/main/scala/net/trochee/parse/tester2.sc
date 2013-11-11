package net.trochee.parse

object tester2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val pf = ParseForest.fromTokens(Seq("hides", "opera"))
                                                  //> pf  : net.trochee.parse.ParseForest = ParseForest(List(Stream(hides[0.98], ?
                                                  //| ), Stream(opera[0.4], ?)))
                                               
  pf.topAnalysis                                  //> res0: Seq[net.trochee.parse.LabeledSpan] = List(hides[0.98], opera[0.4])
  pf.nextAnalyses.toList                          //> res1: List[net.trochee.parse.ParseForest] = List(ParseForest(List(Stream(hid
                                                  //| es[0.4], ?), Stream(opera[0.4]))))
  
  pf.spanningAnalyses.toList                      //> res2: List[net.trochee.parse.LabeledSpan] = List('Hidden(List(hides[0.98], o
                                                  //| pera[0.4])) [0.96], 'Content(List(hides[0.4], opera[0.4])) [0.8])
  
}