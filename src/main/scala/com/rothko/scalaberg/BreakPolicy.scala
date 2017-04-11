package com.rothko.scalaberg

sealed trait BreakPolicy {}
case object BeforeContentBreakPolicy extends BreakPolicy
case object AfterContentBreakPolicy extends BreakPolicy
case class AfterRowBreakPolicy(index: Int) extends BreakPolicy
case class BeforeRowBreakPolicy(index: Int) extends BreakPolicy
