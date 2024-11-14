package zio.inspections

import zio.intellij.inspections.simplifications.SimplifyWhenInspection

abstract class SimplifyWhenInspectionTestBase(replaceWith: String, `ZIO.when`: String => String => String)
    extends ZSimplifyInspectionTest[SimplifyWhenInspection] {

  override protected val hint = s"Replace with $replaceWith"

  def test_when_reference_to_val(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |val b = ZIO.succeed(42)
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (a) b else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) b else ZIO.unit"))
      val result = z(base(`ZIO.when`("a")("b")))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else b$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else b"))
      val result = z(base(`ZIO.when`("a")("b")))
      testQuickFix(text, result, hint)
    }
  }

  def test_when_direct_reference(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (a) ZIO.succeed(42) else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) ZIO.succeed(42) else ZIO.unit"))
      val result = z(base(`ZIO.when`("a")("ZIO.succeed(42)")))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else ZIO.succeed(42)$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else ZIO.succeed(42)"))
      val result = z(base(`ZIO.when`("a")("ZIO.succeed(42)")))
      testQuickFix(text, result, hint)
    }
  }

  def test_when_direct_reference_apply(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (a) ZIO(42) else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) ZIO(42) else ZIO.unit"))
      val result = z(base(`ZIO.when`("a")("ZIO(42)")))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else ZIO(42)$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else ZIO(42)"))
      val result = z(base(`ZIO.when`("a")("ZIO(42)")))
      testQuickFix(text, result, hint)
    }
  }

  def test_when_complex_reference(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |val list = (1 to 5).toList
          |val toZIO: Int => UIO[Int] = UIO.succeed(_)
          |$expr""".stripMargin
    val reference = "ZIO.foreach_(list)(toZIO)"

    locally {
      z(base(s"${START}if (a) $reference else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) $reference else ZIO.unit"))
      val result = z(base(`ZIO.when`("a")(reference)))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else $reference$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else $reference"))
      val result = z(base(`ZIO.when`("a")(reference)))
      testQuickFix(text, result, hint)
    }
  }

  def test_when_method_call_with_multiple_params(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |def b(one: Int, two: Int, three: Int) = ZIO.succeed(42)
          |
          |$expr""".stripMargin
    val methodCall = "b(1, 2, 3)"

    locally {
      z(base(s"${START}if (a) $methodCall else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) $methodCall else ZIO.unit"))
      val result = z(base(`ZIO.when`("a")(methodCall)))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else $methodCall$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else $methodCall"))
      val result = z(base(`ZIO.when`("a")(methodCall)))
      testQuickFix(text, result, hint)
    }
  }

  // ¯\_(ツ)_/¯
  def test_when_with_multiple_negation(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |val b = ZIO.succeed(42)
          |$expr""".stripMargin

    locally {
      z(base(s"${START}if (!(!a)) b else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (!(!a)) b else ZIO.unit"))
      val result = z(base(`ZIO.when`("a")("b")))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!(!(!a))) ZIO.unit else b$END")).assertHighlighted()
      val text   = z(base(s"if (!(!(!a))) ZIO.unit else b"))
      val result = z(base(`ZIO.when`("a")("b")))
      testQuickFix(text, result, hint)
    }
  }

}

class SimplifyWhenInspectionTest extends SimplifyWhenInspectionTestBase(".when", cond => body => s"$body.when($cond)") {

  def test_when_complex_expression_containing_multiple_tokens(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    val complexExpr = """ZIO.succeed(42) *> ZIO.succeed("wrap this")"""

    locally {
      z(base(s"${START}if (a) $complexExpr else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) $complexExpr else ZIO.unit"))
      val result = z(base(s"($complexExpr).when(a)"))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else $complexExpr$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else $complexExpr"))
      val result = z(base(s"($complexExpr).when(a)"))
      testQuickFix(text, result, hint)
    }
  }

  def test_when_complex_expression_containing_for_comprehension(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    val complexExpr =
      """for {
        |  i <- ZIO.succeed(42)
        |  s <- ZIO.succeed("wrap this")
        |} yield s * i""".stripMargin

    locally {
      z(base(s"${START}if (a) $complexExpr else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) $complexExpr else ZIO.unit"))
      val result = z(base(s"($complexExpr).when(a)"))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else $complexExpr$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else $complexExpr"))
      val result = z(base(s"($complexExpr).when(a)"))
      testQuickFix(text, result, hint)
    }
  }

}
class SimplifyZIOWhenInspectionTest
    extends SimplifyWhenInspectionTestBase("ZIO.when", cond => body => s"ZIO.when($cond)($body)") {

  def test_when_complex_expression_containing_multiple_tokens(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    val complexExpr = """ZIO.succeed(42) *> ZIO.succeed("wrap this")"""

    locally {
      z(base(s"${START}if (a) $complexExpr else ZIO.unit$END")).assertHighlighted()
      val text   = z(base(s"if (a) $complexExpr else ZIO.unit"))
      val result = z(base(s"ZIO.when(a)($complexExpr)"))
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else $complexExpr$END")).assertHighlighted()
      val text   = z(base(s"if (!a) ZIO.unit else $complexExpr"))
      val result = z(base(s"ZIO.when(a)($complexExpr)"))
      testQuickFix(text, result, hint)
    }
  }

  def test_when_complex_expression_containing_for_comprehension(): Unit = {
    def base(expr: String): String =
      s"""|val a = true
          |$expr""".stripMargin

    val complexExpr =
      """for {
        |    i <- ZIO.succeed(42)
        |    s <- ZIO.succeed("wrap this")
        |  } yield s * i""".stripMargin

    locally {
      z(base(s"${START}if (a) $complexExpr else ZIO.unit$END")).assertHighlighted()
      val text = z(base(s"if (a) $complexExpr else ZIO.unit"))
      val result = z {
        base {
          s"""ZIO.when(a) {
             |  $complexExpr
             |}""".stripMargin
        }
      }
      testQuickFix(text, result, hint)
    }

    locally {
      z(base(s"${START}if (!a) ZIO.unit else $complexExpr$END")).assertHighlighted()
      val text = z(base(s"if (!a) ZIO.unit else $complexExpr"))
      val result = z {
        base {
          s"""ZIO.when(a) {
             |  $complexExpr
             |}""".stripMargin
        }
      }
      testQuickFix(text, result, hint)
    }
  }

}
