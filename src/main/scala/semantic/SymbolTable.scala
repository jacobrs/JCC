package semantic

import semantic.Symbol.SymbolEntry

class SymbolTable(tableName: String) {

  val name: String = tableName
  var symbols: Seq[SymbolEntry] = Seq.empty

  def addSymbol(symbol: SymbolEntry): Unit = {
    symbols = symbols ++ Seq(symbol)
  }

  def printOutput(): String = {
    var nestedTables = Seq.empty[SymbolTable]
    val header = s"+------------------\n| $name\n+------------------\n"
    var body = ""
    symbols.foreach(s => {
      body += s"| ${s.name} | ${s.kind} | ${printType(s)}\n"
      s.link.fold()(l => nestedTables = nestedTables ++ Seq(l))
    })
    val footer = "+------------------\n\n"
    var output = header + body + footer

    nestedTables.foreach(t => output += t.printOutput())

    output
  }

  def printType(t: SymbolEntry): String = {
    var output = t.dataType
    t.arrayDimensions.foreach(output += "[" + _ + "]")
    output
  }

}
