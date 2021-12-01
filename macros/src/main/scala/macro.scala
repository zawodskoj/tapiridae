import attrs.{json, defaultBody, DefaultBody}
import eio._

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
      case class Security(sec: c.Tree) extends Input
    }

    case class IndexedInput(input: Input, index: Int)

    import c.universe._

    val clsTpe = weakTypeOf[Cls]
    val fTpe = weakTypeOf[F[_]].typeConstructor.dealias

    val pathAttrSym = symbolOf[attrs.path]
    val queryAttrSym = symbolOf[attrs.query]
    val securityAttrSym = symbolOf[attrs.security]

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
          case q"new $attr" if attr.symbol == securityAttrSym => c.inferImplicitValue(appliedType(weakTypeOf[Security[_, _, F]].typeConstructor, WildcardType, paramTpe, fTpe)) match {
            case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find Security instance for type $paramTpe")
            case sec => Input.Security(sec)
          }
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
        val secInputs = inputs.collect { case np: Input.Security => np }

        if (secInputs.size > 1) {
          c.abort(c.enclosingPosition, "Multiple secure endpoints are not supported now")
        }

        val indexedPathInputs = pathInputs.map(p => IndexedInput(p, segmentedPath.collect { case s: PathSegment.Subst => s }.indexWhere(_.name == p.name)))
        val indexedNonPathInputs = nonPathInputs.zipWithIndex.map { case (np, i) => IndexedInput(np, i + indexedPathInputs.size) }
        val indexedSecInputs = secInputs.zipWithIndex.map { case (np, i) => IndexedInput(np, -1) }

        val indexedInputsUnordered = indexedPathInputs ++ indexedSecInputs ++ indexedNonPathInputs

        inputs.map { i => indexedInputsUnordered.find(_.input == i).get }
      }

      case class DecodedOutType(eo: Tree)

      case class DecodedOutTypes(
        async: Boolean,
        isEither: Boolean,
        outType: Option[DecodedOutType],
        errType: Option[DecodedOutType]
      )

      val defBody =  clsTpe.typeSymbol.annotations.map(a => c.typecheck(a.tree.duplicate)).collectFirst {
        case tree if tree.tpe.typeConstructor == weakTypeOf[defaultBody[DefaultBody]].typeConstructor =>
          tree.tpe.typeArgs.head match {
            case t if t.typeSymbol == symbolOf[json] => weakTypeOf[DefTags.JsonDefTag]
            case t => c.abort(c.enclosingPosition, s"Unknown defType $t")
          }
      }

      val outputType = {
        def decodeOutputType(tpe: Type, async: Boolean): DecodedOutTypes = {
          def decodeExactOutputType(tpe: Type): Option[DecodedOutType] = {
            val dealiased = tpe.dealias

            if (dealiased.typeSymbol == symbolOf[Unit]) return None

            c.inferImplicitValue(appliedType(weakTypeOf[ProvidedEndpointOutput[_]].typeConstructor, dealiased)) match {
              case EmptyTree => defBody match {
                case Some(defBody) => c.inferImplicitValue(appliedType(weakTypeOf[EndpointOutputConstructor[_, _]], dealiased, defBody)) match {
                  case EmptyTree => c.abort(c.enclosingPosition, s"Failed to find PEO or EOC for type $tpe (dealiased to $dealiased)")
                  case t => Some(DecodedOutType(q"$t.instance"))
                }
                case None => c.abort(c.enclosingPosition, s"Failed to find PEO and no defaultBody was found")
              }
              case t => Some(DecodedOutType(t))
            }
          }

          if (tpe.typeConstructor.typeSymbol == weakTypeOf[Either[_, _]].typeConstructor.typeSymbol) {
            val List(left, right) = tpe.typeArgs

            DecodedOutTypes(async = async, isEither = true, outType = decodeExactOutputType(right), errType = decodeExactOutputType(left))
          } else {
            DecodedOutTypes(async = async, isEither = false, outType = decodeExactOutputType(tpe), errType = None)
          }
        }

        d.returnType.typeConstructor.dealias match {
          case tc if tc.takesTypeArgs && tc.typeSymbol == fTpe.typeSymbol =>
            if (tc.typeParams.size == 1)
              decodeOutputType(d.returnType.typeArgs.head, async = true)
            else
              c.abort(c.enclosingPosition, "Multi-arg effect type constructors are not supported now")
          case _ =>
            decodeOutputType(d.returnType, async = false)
        }
      }

      println(outputType)

      def tupleMappingTree(hasSec: Boolean) = {
        val args = if (hasSec)
          indexedInputs.map {
            case IndexedInput(_, -1) =>  q"t._1"
            case IndexedInput(_, ix) =>  q"t._2.${TermName("_" + (ix + 1))}"
          }
        else
          indexedInputs.map(i => q"t.${TermName("_" + (i.index + 1))}")
        val rawBody = q"$cls.$methodName(..$args)"
        val monadForF = q"_root_.cats.Monad[$fTpe]"

        val wrappedBody = outputType match {
          case DecodedOutTypes(true, true, _, _) => rawBody
          case DecodedOutTypes(true, false, _, _) => q"monadF.fmap($rawBody)(_.asRight)"
          case DecodedOutTypes(false, true, _, _) => q"monadF.pure($rawBody)"
          case DecodedOutTypes(false, false, _, _) => q"monadF.pure($rawBody.asRight)"
        }

        q"{ import _root_.cats.syntax.either._; val monadF = $monadForF; t => $wrappedBody }"
      }

      val pathInputs = inputs.collect { case p: Input.Path => p }
      val secInputs = inputs.collect { case np: Input.Security => np }
      val nonPathInputs = inputs.collect { case np: Input.NonPath => np }

      val endpointDef = {
        val pathNodes = segmentedPath.map {
          case PathSegment.Raw(r) =>
            Literal(Constant(r))
          case PathSegment.Subst(s) =>
            val tpe = pathInputs.find(p => p.name == s).get.tpe
            q"_root_.sttp.tapir.path[$tpe]($s)"
        }

        val pathInput = pathNodes.reduce((l, r) => q"$l / $r")

        val endpointWithMethod: Tree = q"endpoint.post"

        val withInputs = {
          val withSecInputs = secInputs.foldLeft(endpointWithMethod)((e, i) => q"$e.in(${i.sec}.input).serverLogicForCurrent(${i.sec}.handler)")
          val withPath = q"$withSecInputs.in($pathInput)"

          nonPathInputs.foldLeft(withPath)((e, i) => q"$e.in(${i.tree})")
        }

        val withOutputs = outputType.outType match {
          case Some(out) => q"$withInputs.out(${out.eo})"
          case None => withInputs
        }

        val withErrors = outputType.errType match {
          case Some(err) => q"$withOutputs.errorOut(${err.eo})"
          case None => withOutputs
        }

        withErrors
      }

      val hasSec = secInputs.nonEmpty

      if (hasSec)
        q"$endpointDef.serverLogic(${tupleMappingTree(hasSec)})"
      else
        q"$endpointDef.serverLogic[$fTpe](${tupleMappingTree(hasSec)})"
    }

    println(endpoints)

    q"List(..$endpoints)"
  }
}
