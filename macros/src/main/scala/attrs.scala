import scala.annotation.StaticAnnotation

object attrs {
  final class path(str: String) extends StaticAnnotation
  final class query extends StaticAnnotation
  final class jsonBody extends StaticAnnotation

  trait Controller[F[_]]
}
