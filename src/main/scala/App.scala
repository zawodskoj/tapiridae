import attrs._
import cats.Monad
import sttp.model.StatusCode
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint
import eio.ProvidedEndpointOutput._
import eio.Security

import scala.language.experimental.macros

object App {
  type IO[X] = X

  case class SecureThingy(wtf: String)
  object SecureThingy {
    implicit val security: Security[String, SecureThingy, IO] = new Security[String, SecureThingy, IO] {
      override val input: EndpointInput[String] = auth.bearer[String]()
      override def handler(in: String): IO[Either[Unit, SecureThingy]] = Right(SecureThingy("kek" + in))
    }
  }

  @defaultBody[json]
  class Wtf extends Controller[IO] {

    @path("a/{foo}/c/{qux}")
    def x(foo: String, @query bar: String, qux: Int): IO[Either[StatusCode, String]] = Right("")

    @path("a/{foo}/c/{qux}")
    def y(foo: String, @query bar: String, qux: Int, @security secure: SecureThingy): String = ""

    @path("a/{foo}/c/{qux}")
    def z(foo: String, @query bar: String, qux: Int): List[String] = Nil

    @path("a/{foo}/c/{qux}")
    def zw(foo: String, @query bar: String, qux: Int): Either[StatusCode, Unit] = Left(StatusCode.Ok)
  }

  def process[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[_, _, _, Any, Eff]] = macro makro.processImpl[Cls, Eff]

  def main(args: Array[String]): Unit = {
    val endp = endpoint
      .post
      .in(query[String]("foo"))
      .in(path[String]("bar"))
      .out(plainBody[String])

    val wtf = new Wtf
    val endpm = process[Wtf, IO](wtf)

    println(endp.show)
    println(endpm.map(_.show).mkString(";\n"))
  }
}
