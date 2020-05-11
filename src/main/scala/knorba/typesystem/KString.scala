package knorba.typesystem
import java.io.{InputStream, OutputStream}

import net.kfoundation.{MurmurHash3, UString}


object KString {
  implicit class KStringInterpolation(ctx: StringContext) {
    object k {
      def apply(expr: Any*): KString = new KString(ctx.s(expr:_*))
    }
  }

  val HEADER_SIZE: Int = /*size:*/ 4
  val EMPTY = new KString(UString.EMPTY)

  def readFromBinaryStream(input: InputStream): KString = {
    val size = NumberIO.readInt(input)
    new KString(UString.readUtf8(input, size))
  }

  def of(str: String): KString = of(new UString(str))
  def of(uStr: UString) = new KString(uStr)
}


class KString(private val data: UString) extends KValue {
  private var hash: KLongInt = null

  def this(nativeString: String) = this(new UString(nativeString))

  def get: UString = data
  def toUtf8: Seq[Byte] = data.toUtf8

  def getHashCode: KLongInt = {
    if(hash == null) {
      hash = new KLongInt(MurmurHash3.hash128x64(data.toUtf8)(0))
    }
    hash
  }

  override def getType: KType[_ <: KValue] = KType.STRING

  override def getTotalSizeInOctets: Long =
    KString.HEADER_SIZE + data.getUtf8Length

  override def writeToBinaryStream(output: OutputStream): Unit = {
    NumberIO.writeInt(data.getUtf8Length, output)
    data.printToStream(output)
  }

  override def hashCode(): Int = 31 * data.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case s: KString => s.data.equals(data)
    case _ => false
  }

  override def toString: String = data.toString
}
