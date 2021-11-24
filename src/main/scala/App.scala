import attrs._
import sttp.tapir._
import sttp.tapir.server.ServerEndpoint

import scala.language.experimental.macros

object App {
  type Id[X] = X

  class Wtf extends Controller[Id] {
    @path("a/{foo}/c/{qux}")
    def x(
      foo: String,
      @query bar: String,
      qux: Int
    ): Id[Either[Unit, Unit]] @jsonBody = ???
  }

  def process[Cls <: Controller[Eff], Eff[_]](cls: Cls): List[ServerEndpoint[Any, Eff]] = macro makro.processImpl[Cls, Eff]

  def main(args: Array[String]): Unit = {
    val endp = endpoint
      .post
      .in(query[String]("foo"))
      .in(path[String]("bar"))
      .out(plainBody[String])

    val wtf = new Wtf
    val endpm = process[Wtf, Id](wtf)

    println(endp.show)
    println(endpm.map(_.show))
  }
}
