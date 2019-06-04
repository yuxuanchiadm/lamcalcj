package org.lamcalcj.utils

import org.scalatest.FunSpec
import org.lamcalcj.utils.Trampoline._

class TrampolineTest extends FunSpec {
  describe("Test trampoline") {
    it("Should not stack overflow") {
      def foo(i: Int): Trampoline[Int] = for {
        x <- if (i > 0) More(() => foo(i - 1)) else Done(0)
      } yield x + i
      assertResult(2147450880)(foo(65535).runT)
    }
    it("Should be interruptible") {
      def foo: Trampoline[Nothing] =
        More(() => foo)
      Thread.currentThread().interrupt()
      assertThrows[InterruptedException](foo.interruptibleRunT)
    }
  }
}
