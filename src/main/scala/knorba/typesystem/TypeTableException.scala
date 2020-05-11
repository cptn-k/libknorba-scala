package knorba.typesystem

class TypeTableException(message: String, cause: Throwable)
  extends Exception(message, cause)
{
  def this(message: String) = this(message, null)
}
