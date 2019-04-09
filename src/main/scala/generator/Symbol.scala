package generator

object Symbol {

  case class SymbolEntryWMemory(name: String, kind: String, dataType: String, link: Option[SymbolMemoryTable],
                         arrayDimensions: Seq[Int] = Seq.empty, offset: Int, size: Int)
}
