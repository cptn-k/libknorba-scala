package knorba.typesystem

import java.io.{InputStream, OutputStream}

import scala.annotation.tailrec

 object NumberIO {
  def readByte(input: InputStream): Byte = {
    val v = input.read()
    if(v < 0) {
      throw new UnexpectedEndOfStreamException()
    }
    v.toByte
  }

  def writeToByteArray(input: Long, output: Array[Byte]): Array[Byte] = {
    @tailrec
    def write(i: Int, shift: Int): Unit = if(i >= 0) {
      output(i) = (input >>> shift).toByte
      write(i - 1, shift + 8)
    }
    write(output.length - 1, 0)
    output
  }

  @tailrec
  def write(input: Long, shift: Int, output: OutputStream): Unit =
    if(shift >= 0) {
      output.write((input >>> shift).toByte)
      write(input, shift - 8, output)
    }

  def writeChar(input: Char, output: OutputStream): Unit = write(input, 8, output)
  def writeInt(input: Int, output: OutputStream): Unit = write(input, 24, output)
  def writeLong(input: Long, output: OutputStream): Unit = write(input, 56, output)

  def readFromByteArray(input: Seq[Byte]): Long =
    readLongFromByteArray(input, 0)

  def readIntFromByteArray(input: Seq[Byte], offset: Int): Int =
    readFromByteArray(input, offset, 4).toInt

  def readLongFromByteArray(input: Seq[Byte], offset: Int): Long =
    readFromByteArray(input, offset, 8)

  private def readFromByteArray(input: Seq[Byte], offset: Int, size: Int): Long = {
    @tailrec
    def read(i: Int, shift: Int, result: Long): Long =
      if(i >= 0) {
        read(i - 1, shift + 8,
          result | (input(offset + i) & 0xff).toLong << shift)
      } else {
        result
      }

    read(size - 1, 0, 0)
  }

  def readFromBinaryStream(input: InputStream, size: Int): Long = {
    val v = new Array[Byte](size)
    if(input.read(v, 0, size) < size) {
      throw new UnexpectedEndOfStreamException()
    }
    readFromByteArray(v, 0, size)
  }

  def readInt(input: InputStream): Int   = readFromBinaryStream(input, 4).toInt
  def readLong(input: InputStream): Long = readFromBinaryStream(input, 8)
  def readChar(input: InputStream): Char = readFromBinaryStream(input, 2).toChar
}
