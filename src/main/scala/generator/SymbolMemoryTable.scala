package generator

import generator.Symbol.SymbolEntryWMemory
import semantic.Symbol.SymbolEntry
import semantic.SymbolTable

import scalaz.syntax.std.boolean._

class SymbolMemoryTable(table: SymbolTable, symbolsAboveScope: Seq[SymbolEntryWMemory] = Seq()) {

  val name: String = table.name
  var totalMemory: Int = 0
  var symbols: Seq[SymbolEntryWMemory] = Seq.empty

  var i = 0
  table.symbols.foreach(transformSymbol)

  private def transformSymbol(s: SymbolEntry): Unit = {
    s.kind match {
      case "variable" =>
        parseVariable(s, symbols ++ symbolsAboveScope)
      case "function" =>
        val nt = new SymbolMemoryTable(s.link.getOrElse(
          new SymbolTable("undefined")), symbols ++ symbolsAboveScope)
        addSymbol(s, i, Some(nt), nt.totalMemory)
        i += nt.totalMemory
      case "class" =>
        val nt = new SymbolMemoryTable(s.link.getOrElse(
          new SymbolTable("undefined")), symbols ++ symbolsAboveScope)
        addSymbol(s, i, Some(nt), nt.totalMemory)
        i += nt.totalMemory
      case "parameter" =>
        parseVariable(s, symbols ++ symbolsAboveScope)
      case _ =>
        println(s"[error] unknown kind ${s.kind}")
    }
    totalMemory = i
  }

  def parseVariable(s: SymbolEntry, knownSymbols: Seq[SymbolEntryWMemory]): Unit = {
    s.dataType match {
      case "integer" | "float" =>
        val multiplier = s.arrayDimensions.product
        val sz = ((multiplier * 4) == 0).fold(4, multiplier * 4)
        addSymbol(s, i, None, sz)
        i += sz
      case _ =>
        val multiplier = s.arrayDimensions.product
        val size = findClassSize(s.dataType, knownSymbols)
        val sz = ((multiplier * size) == 0).fold(4, multiplier * size)
        addSymbol(s, i, None, sz)
        i += sz
    }
  }

  def findClassSize(target: String, knownSymbols: Seq[SymbolEntryWMemory]): Int = {
    knownSymbols.find(s => s.kind.equals("class") && s.name.equals(target)).fold(0)(_.link.fold(0)(_.totalMemory))
  }

  def addSymbol(s: SymbolEntry, offset: Int, newLink: Option[SymbolMemoryTable], size: Int): Unit = {
    symbols = symbols ++ Seq(SymbolEntryWMemory(s.name, s.kind, s.dataType, newLink, s.arrayDimensions, offset, size))
  }

  def printOutput(): String = {
    var nestedTables = Seq.empty[SymbolMemoryTable]
    val header = s"+-----------------------------------------------------------------------------------------------+\n| " +
      f"$name%-94s|\n+-----------------------------------------------------------------------------------------------+\n"
    var body = ""
    symbols.foreach(s => {
      body += f"| ${s.name}%-20s | ${s.kind}%-15s | ${printType(s)}%-30s | ${s.offset}%-8s | ${s.size}%8s |\n"
      s.link.fold()(l => nestedTables = nestedTables ++ Seq(l))
    })
    val footer = "+-----------------------------------------------------------------------------------------------+\n\n"
    var output = header + body + footer

    nestedTables.foreach(t => output += t.printOutput())

    output
  }

  def printType(t: SymbolEntryWMemory): String = {
    var output = t.dataType
    t.arrayDimensions.foreach(output += "[" + _ + "]")
    output
  }

}
