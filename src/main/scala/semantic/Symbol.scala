package semantic

object Symbol {

  case class SymbolEntry(name: String, kind: String, dataType: String, link: Option[SymbolTable])

}
