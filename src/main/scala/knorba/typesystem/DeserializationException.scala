package knorba.typesystem

import java.io.IOException

class DeserializationException(message: String, cause: Throwable)
  extends IOException(message, cause)
{
  def this() = this(null, null)
  def this(message: String) = this(message, null)
}

class UnexpectedEndOfStreamException() extends DeserializationException()