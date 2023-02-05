import org.bson.*

trait EncoderSyntax:
  inline def apply[T](using encoder: Encoder[T]): Encoder[T] = encoder
  inline def encode[T](value: T)(using encoder: Encoder[T]): BsonValue = encoder.encode(value)
  extension [T](value: T)(using encoder: Encoder[T]) inline def toBson: BsonValue = encoder.encode(value)

trait EncoderInstances:

  given Encoder[BsonValue] = b => b

  given Encoder[String] = s => new BsonString(s)
  given Encoder[Boolean] = b => new BsonBoolean(b)
  given Encoder[Int] = i => new BsonInt32(i)
  given Encoder[Long] = l => new BsonInt64(l)
  given Encoder[Double] = d => new BsonDouble(d)
  given Encoder[Array[Byte]] = b => new BsonBinary(b)
  given Encoder[org.bson.types.ObjectId] = o => new BsonObjectId(o)
  given Encoder[org.bson.types.Decimal128] = d => new BsonDecimal128(d)

trait DecoderSyntax:
  inline def apply[T](using decoder: Decoder[T]): Decoder[T] = decoder
  inline def decode[T](bson: BsonValue)(using decoder: Decoder[T]): Option[T] = decoder.decode(bson)
  extension (bson: BsonValue) inline def toOption[T](using decoder: Decoder[T]): Option[T] = decoder.decode(bson)

trait DecoderInstances:

  given Decoder[BsonValue] = bson => Some(bson)

  given Decoder[String] = bson => Some(bson).collect { case bs: BsonString => bs.getValue() }
  given Decoder[Boolean] = bson => Some(bson).collect { case bb: BsonBoolean => bb.getValue() }
  given Decoder[Int] = bson => Some(bson).collect { case bi: BsonInt32 => bi.getValue() }
  given Decoder[Long] = bson => Some(bson).collect { case bi: BsonInt64 => bi.getValue() }
  given Decoder[Double] = bson => Some(bson).collect { case bd: BsonDouble => bd.getValue() }
  given Decoder[Array[Byte]] = bson => Some(bson).collect { case bb: BsonBinary => bb.getData() }
  given Decoder[org.bson.types.ObjectId] = bson => Some(bson).collect { case bo: BsonObjectId => bo.getValue() }
  given Decoder[org.bson.types.Decimal128] = bson => Some(bson).collect { case bd: BsonDecimal128 => bd.getValue() }
