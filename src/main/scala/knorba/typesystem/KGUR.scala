package knorba.typesystem
import java.io.{InputStream, OutputStream}

import knorba.typesystem.KGUR.{AppID, URef}


object KGUR {
  class AppID private[KGUR] (octets: Long) {
    def writeToBinaryStream(output: OutputStream): Unit =
      NumberIO.writeLong(octets, output)
  }

  class URef(val reserved: Byte, val manager: Byte, val index: Int, val key: Char) {
    def this(manager: Byte, index: Int, key: Char) =
      this(0, manager, index, key)

    def writeToBinaryStream(output: OutputStream): Unit = {
      output.write(0)
      output.write(manager)
      NumberIO.writeInt(index, output)
      NumberIO.writeChar(key, output)
    }
  }

  def readAppIdFromBinaryStream(input: InputStream): AppID = {
    new AppID(NumberIO.readLong(input))
  }

  def readURefFromBinaryStream(input: InputStream): URef = new URef(
    NumberIO.readByte(input),
    NumberIO.readByte(input),
    NumberIO.readInt(input),
    NumberIO.readChar(input))

  val NULL = new KGUR(new AppID(0), new URef(0, 0, 0))
}


class KGUR(val appId: AppID, val ref: URef)
  extends KValue
{
  override def getType: KType[_ <: KValue] = KType.GUR
  override def getTotalSizeInOctets: Long = KType.GUR.sizeInOctets
  override def writeToBinaryStream(output: OutputStream): Unit = {
    appId.writeToBinaryStream(output)
    ref.writeToBinaryStream(output)
  }
}
