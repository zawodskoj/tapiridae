import scala.annotation.StaticAnnotation

object attrs {
  sealed trait DefaultBody

  final class path(str: String) extends StaticAnnotation
  final class query extends StaticAnnotation
  final class defaultBody[d <: DefaultBody] extends StaticAnnotation

  final class json extends StaticAnnotation with DefaultBody

  trait Controller[F[_]]
}
