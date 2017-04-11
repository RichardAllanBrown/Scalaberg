package com.rothko.scalaberg

class TableBuilder {

  def buildTable(values: Seq[Seq[String]], options: TableBuilderOptions = TableBuilderOptions.default): String = {
    calculateColumnsNeeded(values) match {
      case Some(colCount) => buildTableFromEven(stretchRowsToMax(values, colCount), options)
      case None => ""
    }
  }

  private def stretchRowsToMax(values: Seq[Seq[String]], colCount: Int): Seq[Seq[String]] = values.map(_.padTo(colCount, ""))

  private def buildTableFromEven(values: Seq[Seq[String]], options: TableBuilderOptions) = {
    val maxWidths = calculateMaxWidths(values)
    val sb = new StringBuilder()

    if (options.breakPolicies.contains(BeforeContentBreakPolicy)) {
      sb.append(buildSeparatorRow(maxWidths))
    }

    for ((row, index) <- values.zipWithIndex) {
      if (options.breakPolicies.contains(BeforeRowBreakPolicy(index))) {
        sb.append(buildSeparatorRow(maxWidths))
      }

      sb.append(buildDataRow(maxWidths, row))

      if (options.breakPolicies.contains(AfterRowBreakPolicy(index))) {
        sb.append(buildSeparatorRow(maxWidths))
      }
    }

    if (options.breakPolicies.contains(AfterContentBreakPolicy)) {
      sb.append(buildSeparatorRow(maxWidths))
    }

    sb.toString
  }

  private def calculateColumnsNeeded(values: Seq[Seq[String]]) = values.map(_.length).reduceOption(_ max _)

  private def calculateMaxWidths(standardizedRows: Seq[Seq[String]]): Seq[Int] = {
    val initial = Seq.fill(standardizedRows.head.length)(0).zipWithIndex

    standardizedRows.foldLeft(initial) { case (currentMax, row) =>
      currentMax.map { case (currMaxLength, index) =>
        (row.lift(index).map(_.length).getOrElse(0) max currMaxLength, index)
      }
    }.map { case (maxLength, _) => maxLength }
  }

  private def buildSeparatorRow(widestColumnWidths: Seq[Int]): StringBuilder = {
    val rowBuilder = new StringBuilder()
    rowBuilder.append("+")
    for (colWidth <- widestColumnWidths) {
      0 to (colWidth+2) foreach { _ => rowBuilder.append('-') }
      rowBuilder.append("+")
    }
    rowBuilder.append("\n")
  }

  private def buildDataRow(widestColumnWidths: Seq[Int], row: Seq[String]): StringBuilder = {
    val rowBuilder = new StringBuilder()
    rowBuilder.append('|')
    for ((col, widestCol) <- row.zip(widestColumnWidths)) {
      rowBuilder.append(' ')
      rowBuilder.append(col)
      col.length to (widestCol+1) foreach { _ => rowBuilder.append(' ') }
      rowBuilder.append('|')
    }
    rowBuilder.append('\n')
  }

}
