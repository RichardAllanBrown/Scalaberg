package com.rothko.scalaberg

object ScalabergExamplesApp extends App {

  val tableData = Seq(
    Seq("Quarter", "Some Field", "Other", "Month"),
    Seq("Quarter 1", "12", "43", "September"),
    Seq("Quarter 2", "12", "43", "October"),
    Seq("Quarter 3", "12", "43"),
    Seq("Quarter 4", "12", "43", "Nov")
  )

  println("Starting...")
  val tb = new TableBuilder()
  val table = new TableBuilder().buildTable(tableData)
  print(table)
  Console.in.readLine()
}
