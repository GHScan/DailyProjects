import lexical.{TokenBuilder, StringCharSource, TableDrivenScannerBuilder}

object Main extends App {

  val tokenBuilder = new TableDrivenScannerBuilder()
    .token("IDENT", """\w+""")
    .token("INT", """\d+""")
    .token("def", """def""")
    .token("=", """=""")
    .token("(", """\(""")
    .token(")", """\)""")
    .token("WS", """[\t\n\r ]+""")

  val scanner = tokenBuilder.create(new StringCharSource("def func(abc, def) = println(abc, def)"), new TokenBuilder())
  while (scanner.hasNext) {
    println(scanner.next())
  }
}
