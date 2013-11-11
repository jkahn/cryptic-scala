package net.trochee

package object parse {
  type Token = String
  type Position = Int
  type Label = Symbol
  type Probability = Double
  type AnalysisQueue = Stream[LabeledSpan]
}