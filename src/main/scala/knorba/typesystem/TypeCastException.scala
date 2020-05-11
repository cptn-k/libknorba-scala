package knorba.typesystem

class TypeCastException(message: String, cause: Throwable)
  extends Error(message, cause)
{
  def this(from: KType[_], to: KType[_]) =
    this(s"Cannot case value of type ${from.name} to type ${to.name}", null)

  def this(message: String) = this(message, null)
}