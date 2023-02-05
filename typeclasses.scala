import magnolia1.*
import org.bson.*

trait Encoder[T]:
  def encode(value: T): BsonValue

object Encoder extends AutoDerivation[Encoder] with EncoderSyntax with EncoderInstances:

  override def join[T](caseClass: CaseClass[Encoder, T]): Encoder[T] = value =>
    caseClass.params.foldLeft(new BsonDocument) { (doc, param) =>
      val bson = param.typeclass.encode(param.deref(value))
      doc.append(param.label, bson)
    }

  override def split[T](sealedTrait: SealedTrait[Encoder, T]): Encoder[T] = value =>
    sealedTrait.choose(value) { subtype =>
      val subtypeValue = subtype.cast(value)
      subtype.typeclass.encode(subtypeValue)
    }

trait Decoder[T]:
  def decode(bson: BsonValue): Option[T]

object Decoder extends AutoDerivation[Decoder] with DecoderSyntax with DecoderInstances:

  override def join[T](caseClass: CaseClass[Decoder, T]): Decoder[T] = value =>
    caseClass.constructMonadic { params =>
      val bson = value.asDocument().get(params.label)
      params.typeclass.decode(bson)
    }

  override def split[T](sealedTrait: SealedTrait[Decoder, T]): Decoder[T] = value =>
    sealedTrait.subtypes
      .map { subtype => subtype.typeclass.decode(value) }
      .collectFirst { case Some(decoded) => decoded }
