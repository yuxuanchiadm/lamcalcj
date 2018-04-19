package org.lamcalcj.parser.lexical

import org.lamcalcj.parser.lexical.Location._

case class Token(kind: Kind, location: Location, image: String)
