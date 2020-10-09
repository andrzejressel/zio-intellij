package zio.macros

import intellij.testfixtures.RichStr
import org.jetbrains.plugins.scala.base.libraryLoaders.{IvyManagedLoader, LibraryLoader}
import org.junit.Assert._

class ModulePatternAccessibleTest extends MacrosTest {

  override def librariesLoaders: Seq[LibraryLoader] =
    super.librariesLoaders :+
      IvyManagedLoader(zioOrg %% "zio-streams" % zioVersion, zioOrg %% "zio-macros" % zioVersion)

  override protected val code =
    s"""
import zio._
import zio.blocking.Blocking
import zio.macros.accessible
import zio.stream.{ZSink, ZStream}

@accessible
object E${CARET}xample {
  type Environment = Blocking

  type EIO[+T] = ZIO[Environment, Nothing, T]

  sealed trait Foo { val value: String }
  final case class Bar(value: String) extends Foo
  final case class Wrapped[T](value: T)

  trait Service {
    val v: EIO[Boolean]
    def m0: EIO[Unit]
    def m1(s: String): EIO[Int]
    def m2[T](s2: String = "")(p: (T, Int))(i2: Int*): UIO[Double]
    def m3[T <: Foo](t: Wrapped[T]): IO[String, List[T]]

    val defaultPureValue: EIO[Boolean] = v
    def defaultPureMethod(s: String): EIO[Int] = m1

    val vManaged: Managed[String, Foo]
    def mManaged(s: String): ZManaged[Any, Nothing, Bar]

    val vNonZIO: Boolean
    def m0NonZIO: Unit
    def m1NonZIO(s: String): Int
    def m2NonZIO[T](s2: String = "")(p: (T, Int))(i2: Int*): Double
    def stream(n: Int): ZStream[Any, String, Int]
    def sink(n: Int): ZSink[Any, String, Int, Int, List[Int]]

    val defaultImpureValue: Boolean = vNonZIO
    def defaultImpureMethod(s: String): Int = m1NonZIO(s)
  }
}
"""

  def test_generates_accessor_value_for_ZIO_field(): Unit =
    assertEquals(
      "val v: zio.ZIO[zio.Has[Example.Service] with Example.Environment, Nothing, Boolean] = " +
        "zio.ZIO.accessM(_.get[Example.Service].v)",
      field("v").getText
    )

  def test_generates_accessor_function_for_ZIO_method_without_arguments(): Unit =
    assertEquals(
      "def m0: zio.ZIO[zio.Has[Example.Service] with Example.Environment, Nothing, Unit] = " +
        "zio.ZIO.accessM(_.get[Example.Service].m0)",
      method("m0").getText
    )

  def test_generates_accessor_function_for_ZIO_method_with_argument(): Unit =
    assertEquals(
      "def m1(s: String): zio.ZIO[zio.Has[Example.Service] with Example.Environment, Nothing, Int] = " +
        "zio.ZIO.accessM(_.get[Example.Service].m1(s))",
      method("m1").getText
    )

  def test_generates_accessor_function_for_generic_ZIO_method_with_multiple_arg_lists_default_args_and_varargs(): Unit =
    assertEquals(
      """def m2[T](s2: String = "")(p: (T, Int))(i2: Int*): zio.ZIO[zio.Has[Example.Service], Nothing, Double] = """ +
        "zio.ZIO.accessM(_.get[Example.Service].m2[T](s2)(p)(i2: _*))",
      method("m2").getText
    )

  def test_generates_accessor_function_for_generic_ZIO_method_with_type_constraints(): Unit =
    assertEquals(
      "def m3[T <: Example.Foo](t: Example.Wrapped[T]): zio.ZIO[zio.Has[Example.Service], String, List[T]] = " +
        "zio.ZIO.accessM(_.get[Example.Service].m3[T](t))",
      method("m3").getText
    )

  def test_generates_accessor_value_for_ZIO_field_with_default_implementation(): Unit =
    assertEquals(
      "val defaultPureValue: zio.ZIO[zio.Has[Example.Service] with Example.Environment, Nothing, Boolean] = " +
        "zio.ZIO.accessM(_.get[Example.Service].defaultPureValue)",
      field("defaultPureValue").getText
    )

  def test_generates_accessor_function_for_ZIO_method_with_argument_with_default_implementation(): Unit =
    assertEquals(
      "def defaultPureMethod(s: String): zio.ZIO[zio.Has[Example.Service] with Example.Environment, Nothing, Int] = " +
        "zio.ZIO.accessM(_.get[Example.Service].defaultPureMethod(s))",
      method("defaultPureMethod").getText
    )

  def test_generates_accessor_value_for_non_ZIO_field(): Unit =
    assertEquals(
      "val vNonZIO: zio.ZIO[zio.Has[Example.Service], Throwable, Boolean] = " +
        "zio.ZIO.access(_.get[Example.Service].vNonZIO)",
      field("vNonZIO").getText
    )

  def test_generates_accessor_function_for_non_ZIO_method_without_arguments(): Unit =
    assertEquals(
      "def m0NonZIO: zio.ZIO[zio.Has[Example.Service], Throwable, Unit] = " +
        "zio.ZIO.access(_.get[Example.Service].m0NonZIO)",
      method("m0NonZIO").getText
    )

  def test_generates_accessor_function_for_non_ZIO_method_with_argument(): Unit =
    assertEquals(
      "def m1NonZIO(s: String): zio.ZIO[zio.Has[Example.Service], Throwable, Int] = " +
        "zio.ZIO.access(_.get[Example.Service].m1NonZIO(s))",
      method("m1NonZIO").getText
    )

  def test_generates_accessor_function_for_generic_non_ZIO_method_with_multiple_arg_lists_default_args_and_varargs()
    : Unit =
    assertEquals(
      """def m2NonZIO[T](s2: String = "")(p: (T, Int))(i2: Int*): zio.ZIO[zio.Has[Example.Service], Throwable, Double] = """ +
        "zio.ZIO.access(_.get[Example.Service].m2NonZIO[T](s2)(p)(i2: _*))",
      method("m2NonZIO").getText
    )

  def test_generates_accessor_function_for_ZIO_method_returning_stream(): Unit =
    assertEquals(
      "def stream(n: Int): zio.stream.ZStream[zio.Has[Example.Service], String, Int] = " +
        "zio.stream.ZStream.accessStream(_.get[Example.Service].stream(n))",
      method("stream").getText
    )

  def test_generates_accessor_function_for_ZIO_method_returning_sink(): Unit =
    assertEquals(
      "def sink(n: Int): zio.stream.ZSink[zio.Has[Example.Service], String, Int, Int, List[Int]] = " +
        "zio.stream.ZSink.accessSink(_.get[Example.Service].sink(n))",
      method("sink").getText
    )

  def test_generates_accessor_value_for_non_ZIO_field_with_default_implementation(): Unit =
    assertEquals(
      "val defaultImpureValue: zio.ZIO[zio.Has[Example.Service], Throwable, Boolean] = " +
        "zio.ZIO.access(_.get[Example.Service].defaultImpureValue)",
      field("defaultImpureValue").getText
    )

  def test_generates_accessor_function_for_non_ZIO_method_with_argument_with_default_implementation(): Unit =
    assertEquals(
      "def defaultImpureMethod(s: String): zio.ZIO[zio.Has[Example.Service], Throwable, Int] = " +
        "zio.ZIO.access(_.get[Example.Service].defaultImpureMethod(s))",
      method("defaultImpureMethod").getText
    )

  def test_generates_accessor_value_for_managed_field(): Unit =
    assertEquals(
      "val vManaged: zio.ZManaged[zio.Has[Example.Service], String, Example.Foo] = " +
        "zio.ZManaged.accessManaged(_.get[Example.Service].vManaged)",
      field("vManaged").getText
    )

  def test_generates_accessor_function_for_method_returning_managed(): Unit =
    assertEquals(
      "def mManaged(s: String): zio.ZManaged[zio.Has[Example.Service], Nothing, Example.Bar] = " +
        "zio.ZManaged.accessManaged(_.get[Example.Service].mManaged(s))",
      method("mManaged").getText
    )

}
