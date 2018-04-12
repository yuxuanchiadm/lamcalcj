package org.lamcalcj.pretty

case class Symbols(
  symbolGroupBegin: String = "(",
  symbolGroupEnd: String = ")",
  symbolVariableBegin: String = "",
  symbolVariableEnd: String = "",
  symbolAbstractionBegin: String = "λ",
  symbolAbstractionEnd: String = "",
  symbolAbstractionSeparator: String = "",
  symbolArgumentsBegin: String = "",
  symbolArgumentsEnd: String = ".",
  symbolArgumentsSeparator: String = " ",
  symbolApplyBegin: String = "",
  symbolApplyEnd: String = "",
  symbolApplySeparator: String = " ")
