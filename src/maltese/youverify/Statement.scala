package maltese.youverify

import maltese.smt.{SMTExpr, SMTSymbol, SMTType}

trait Statement



case class Program(name: String, functions: ???, )

case class Function(name: String, )

// TODO: should record also be a type?
case class Field(name: String, tpe: SMTType)
case class Record(name: String, fields: List[Field])

case class Declare(sym: SMTSymbol)
case class Assign(sym: String, rhs: SMTExpr)

