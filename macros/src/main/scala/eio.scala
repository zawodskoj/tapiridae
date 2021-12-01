import io.circe._
import sttp.model.StatusCode
import sttp.tapir._
import sttp.tapir.json.circe._

object eio {
  type ProvidedEndpointInput[T] = EndpointInput[T] { type Provided = true }
  type ProvidedEndpointOutput[T] = EndpointOutput[T] { type Provided = true }
  object ProvidedEndpointOutput {
    implicit val psc: ProvidedEndpointOutput[StatusCode] = statusCode.asInstanceOf[ProvidedEndpointOutput[StatusCode]]
  }
  type ProvidedCodec[L, H, +CF <: CodecFormat] = sttp.tapir.Codec[L, H, CF] { type Provided = true }

  trait EndpointInputConstructor[V, DefTag] {
    def instance: ProvidedEndpointInput[V]
  }
  object EndpointInputConstructor {
    implicit def constructorForJsonTag[V: Decoder: Encoder: Schema]: EndpointInputConstructor[V, DefTags.JsonDefTag] = new EndpointInputConstructor[V, DefTags.JsonDefTag] {
      override val instance: ProvidedEndpointInput[V] = jsonBody[V].asInstanceOf[ProvidedEndpointInput[V]]
    }
  }

  trait EndpointOutputConstructor[V, DefTag] {
    def instance: ProvidedEndpointOutput[V]
  }
  object EndpointOutputConstructor {
    implicit def constructorForJsonTag[V: Decoder: Encoder: Schema]: EndpointOutputConstructor[V, DefTags.JsonDefTag] = new EndpointOutputConstructor[V, DefTags.JsonDefTag] {
      override val instance: ProvidedEndpointOutput[V] = jsonBody[V].asInstanceOf[ProvidedEndpointOutput[V]]
    }
  }

  object DefTags {
    trait JsonDefTag
  }
}
