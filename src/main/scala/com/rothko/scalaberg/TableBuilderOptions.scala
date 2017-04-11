package com.rothko.scalaberg

case class TableBuilderOptions(breakPolicies: Set[BreakPolicy]) {
  def withBreakPolicy(breakPolicy: BreakPolicy): TableBuilderOptions = {
    copy(breakPolicies + breakPolicy)
  }
}

object TableBuilderOptions {
  val default: TableBuilderOptions = {
    val defaultBreakPolicies = Set[BreakPolicy](BeforeContentBreakPolicy, AfterContentBreakPolicy, AfterRowBreakPolicy(0))
    TableBuilderOptions(defaultBreakPolicies)
  }
}


