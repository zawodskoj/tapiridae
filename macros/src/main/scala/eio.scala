import io.circe._
import sttp.model.StatusCode
import sttp.tapir._
import sttp.tapir.json.circe._

object eio {
  case class ProvidedEndpointInput[T](instance: EndpointInput[T]) extends AnyVal
  case class ProvidedEndpointOutput[T](instance: EndpointOutput[T]) extends AnyVal
  object ProvidedEndpointOutput {
    implicit val psc: ProvidedEndpointOutput[StatusCode] = ProvidedEndpointOutput(statusCode)
  }
  type ProvidedCodec[L, H, +CF <: CodecFormat] = sttp.tapir.Codec[L, H, CF] { type Provided = true }

  trait EndpointInputConstructor[V, DefTag] {
    def instance: ProvidedEndpointInput[V]
  }
  object EndpointInputConstructor {
    implicit def constructorForJsonTag[V: Decoder: Encoder: Schema]: EndpointInputConstructor[V, DefTags.JsonDefTag] = new EndpointInputConstructor[V, DefTags.JsonDefTag] {
      override val instance: ProvidedEndpointInput[V] = ProvidedEndpointInput(jsonBody[V])
    }
  }

  trait EndpointOutputConstructor[V, DefTag] {
    def instance: EndpointOutput[V]
  }
  object EndpointOutputConstructor {
    implicit def constructorForJsonTag[V: Decoder: Encoder: Schema]: EndpointOutputConstructor[V, DefTags.JsonDefTag] = new EndpointOutputConstructor[V, DefTags.JsonDefTag] {
      override val instance: EndpointOutput[V] = jsonBody[V]
    }
  }

  object DefTags {
    trait JsonDefTag
  }

  trait Security[VIn, VOut, F[_]] {
    val input: EndpointInput[VIn]
    def handler(in: VIn): F[Either[Unit, VOut]]
  }
}
