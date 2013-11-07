package net.trochee.parse

object tester2 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  val pf = ParseForest(Seq("hides", "opera"))     //> pf  : net.trochee.parse.ParseForest = net.trochee.parse.ParseForest@4229ab3e
                                                  //| 
  pf.spans.head                                   //> java.lang.StackOverflowError
                                                  //| 	at scala.collection.immutable.Stream$Cons.tail(Stream.scala:1083)
                                                  //| 	at scala.collection.immutable.Stream$Cons.tail(Stream.scala:1077)
                                                  //| 	at scala.collection.immutable.Stream$$anonfun$flatMap$1.apply(Stream.sca
                                                  //| la:450)
                                                  //| 	at scala.collection.immutable.Stream$$anonfun$flatMap$1.apply(Stream.sca
                                                  //| la:450)
                                                  //| 	at scala.collection.immutable.Stream.append(Stream.scala:237)
                                                  //| 	at scala.collection.immutable.Stream$$anonfun$append$1.apply(Stream.scal
                                                  //| a:237)
                                                  //| 	at scala.collection.immutable.Stream$$anonfun$append$1.apply(Stream.scal
                                                  //| a:237)
                                                  //| 	at scala.collection.immutable.Stream$Cons.tail(Stream.scala:1085)
                                                  //| 	at scala.collection.immutable.Stream$Cons.tail(Stream.scala:1077)
                                                  //| 	at scala.collection.immutable.Stream$$anonfun$append$1.apply(Stream.scal
                                                  //| a:237)
                                                  //| 	at scala.collection.immutable.Stream$$anonfun$append$1.apply(Stream.scal
                                                  //| a:237)
                                                  //| 	at scala.collection.immutable.Stream$Cons.tail(Stream.scala:1085)
                                                  //| 	at s
                                                  //| Output exceeds cutoff limit.
  
  
}