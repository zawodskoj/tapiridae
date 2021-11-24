import scala.reflect.macros.blackbox

object makro {
  def processImpl[Cls: c.WeakTypeTag, F[_]](c: blackbox.Context)(cls: c.Expr[Cls])(implicit _f: c.WeakTypeTag[F[_]]): c.Tree = {
    sealed trait PathSegment
    object PathSegment {
      case class Raw(value: String) extends PathSegment
      case class Subst(name: String) extends PathSegment
    }

    sealed trait Input
    object Input {
      case class Path(name: String, tpe: c.Type) extends Input
      case class NonPath(tree: c.Tree) extends Input
    }

    case class IndexedInput(input: Input, index: Int)

    import c.universe._

    val clsTpe = weakTypeOf[Cls]
    val fTpe = weakTypeOf[F[_]].typeConstructor

    val pathAttrSym = symbolOf[attrs.path]
    val queryAttrSym = symbolOf[attrs.query]

    val endpoints = clsTpe.decls.collect {
      case d if d.isMethod && d.isPublic && !d.isConstructor && d.asMethod.paramLists.size == 1 => d.asMethod
    }.map { d =>
      val methodName = d.name

      val paths = d.annotations.map(_.tree).collect {
        case q"new $attr($path)" if attr.symbol == pathAttrSym =>
          path match {
            case Literal(Constant(c: String)) => c
            case _ => c.abort(c.enclosingPosition, "Non-literal path value")
          }
      }

      val rawPath = paths match {
        case List(path) => path
        case Nil => c.abort(c.enclosingPosition, "Path was not defined")
        case _ => c.abort(c.enclosingPosition, "Path was defined more than once")
      }

      val segmentedPath = rawPath.split('/').map {
        case s"{$name}" => PathSegment.Subst(name)
        case s => PathSegment.Raw(s)
      }

      val inputs = d.paramLists.head.map { param =>
        val paramTpe = param.typeSignature
        val paramName = param.name.decodedName.toString

        val inputAnnotations: List[Input] = param.annotations.map(_.tree).collect {
          case q"new $attr" if attr.symbol == queryAttrSym => Input.NonPath(q"_root_.sttp.tapir.query[$paramTpe]($paramName)")
        }

        inputAnnotations match {
          case List(input) => input
          case Nil => Input.Path(paramName, paramTpe)
          case _ => c.abort(c.enclosingPosition, "Input type was defined more than once")
        }
      }

      if (segmentedPath.collect { case PathSegment.Subst(n) => n }.toSet != inputs.collect { case Input.Path(name, _) => name }.toSet) {
        c.abort(c.enclosingPosition, "Path segments and path arguments do not match")
      }

      val indexedInputs = {
        val pathInputsCount = segmentedPath.count(_.isInstanceOf[PathSegment.Subst])

        val pathInputs = inputs.collect { case p: Input.Path => p }
        val nonPathInputs = inputs.collect { case np: Input.NonPath => np }

        val indexedPathInputs = pathInputs.map(p => IndexedInput(p, segmentedPath.collect { case s: PathSegment.Subst => s }.indexWhere(_.name == p.name)))
        val indexedNonPathInputs = nonPathInputs.zipWithIndex.map { case (np, i) => IndexedInput(np, i + pathInputsCount) }

        val indexedInputsUnordered = indexedPathInputs ++ indexedNonPathInputs

        inputs.map { i => indexedInputsUnordered.find(_.input == i).get }
      }

      val tupleMappingTree = {
        val args = indexedInputs.map(i => q"t.${TermName("_" + (i.index + 1))}")

        q"{ t => $cls.$methodName(..$args) }"
      }

      val endpointDef = {
        val pathInputs = inputs.collect { case p: Input.Path => p }
        val nonPathInputs = inputs.collect { case np: Input.NonPath => np }

        val pathNodes = segmentedPath.map {
          case PathSegment.Raw(r) =>
            Literal(Constant(r))
          case PathSegment.Subst(s) =>
            val tpe = pathInputs.find(p => p.name == s).get.tpe
            q"_root_.sttp.tapir.path[$tpe]($s)"
        }

        val pathInput = pathNodes.reduce((l, r) => q"$l / $r")

        val endpointWithPath = q"endpoint.post.in($pathInput)"

        nonPathInputs.foldLeft(endpointWithPath)((e, i) => q"$e.in(${i.tree})")
      }

      q"$endpointDef.serverLogic[$fTpe]($tupleMappingTree)"
    }

    println(endpoints)

    q"List(..$endpoints)"
  }
}
