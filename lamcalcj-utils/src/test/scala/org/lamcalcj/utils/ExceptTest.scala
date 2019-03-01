package org.lamcalcj.utils

import org.scalatest.FunSpec
import org.lamcalcj.ast.Lambda._
import org.lamcalcj.utils.Except._

class ExceptTest extends FunSpec {
  describe("Test recursion") {
    it("Should not stack overflow") {
      def foo(i: Int): Except[String, Int] = for {
        x <- if (i > 0) recursive(() => foo(i - 1)) else unit[String, Int](0)
      } yield x + i
      assertResult(Right(2147450880))(foo(65535).runExcept)
    }
  }
}
