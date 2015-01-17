package parsing

abstract class LLParser(_grammar : Grammar) extends IParser {
  val grammar = _grammar.removeLeftRecursion().leftFactoring()

  private def bindPredicateArray() : Unit = {
    for (nt <- grammar.nonTerms) {
      nt.context = Array.tabulate(grammar.maxTermID + 1) { tid =>
        nt.productions.filter(grammar.firstPlusMap(_).contains(tid))
      }
    }
  }
  bindPredicateArray()

  val predictable = grammar.nonTerms.forall(nt => nt.context.asInstanceOf[Array[List[IProduction]]].forall(_.length <= 1))

  override def toString =
    s"""predictable=$predictable\n${
      (for (nt <- grammar.nonTerms)
      yield s"\t$nt: (predictable=${nt.context.asInstanceOf[Array[List[IProduction]]].forall(_.length <= 1)})\n" +
          s"${nt.productions.map(p => s"\t\t$p:\n\t\t\t${grammar.firstPlusMap(p).map(grammar.id2Term).mkString(",")}").mkString("\n")}").mkString("\n\n")
    }
     """
}
