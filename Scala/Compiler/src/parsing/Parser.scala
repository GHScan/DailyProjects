package parsing

import scala.collection.immutable

trait IParser {
  var errors = immutable.Queue[String]()
  def name : String
  def parse(scanner : Iterator[lexical.Token]) : Any
}

trait IParserFactory {
  def name : String
  def create(grammar : Grammar, reportConflict : Boolean) : IParser
  def ll : Boolean
  def lr : Boolean = !ll
}

object ParserFactory {
  val instances = List(
    TableDrivenLL1ParserFactory,
    TableDrivenLLBacktrackingParserFactory,
    TableDrivenLR0ParserFactory,
    TableDrivenSLRParserFactory,
    TableDrivenLALRParserFactory,
    TableDrivenLR1ParserFactory)

  val names : List[String] = instances.map(_.name)
  def get(name : String) : IParserFactory = instances.filter(_.name == name).head
}