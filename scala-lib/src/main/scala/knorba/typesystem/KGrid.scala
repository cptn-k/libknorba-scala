package knorba.typesystem

import java.io.{InputStream, OutputStream}

object KGrid {
  class Dims(private val elements: Int*) extends Iterable[Int] {
    private val rank = elements.length

    def ordinalForIndex(index: Seq[Int]): Int = {
      var ordinal = 0
      var p = 1
      Range(0, rank).foreach(i => {
        ordinal += index(i) * p
        p = p * elements(i)
      })
      ordinal
    }

    def get(i: Int): Int = elements(i)
    def getNumberOfItems: Int = elements.product
    def getRank: Byte = rank.toByte

    override def iterator: Iterator[Int] = elements.iterator

    override def equals(obj: Any): Boolean = obj match {
      case d: Dims => d.elements.equals(elements)
      case _ => false
    }

    override def toString(): String = indexToString(elements)
  }

  val HEADER_SIZE = 1

  private def indexToString(idx: Seq[Int]) = idx.mkString("(", ", ", ")")

  def readFromBinaryStream(t: KGridType, input: InputStream, tt: TypeTable): KGrid = {
    val rank = NumberIO.readByte(input)

    val dims = new Dims(Range(0, rank).map(_ =>
      NumberIO.readInt(input)):_*)

    val et = t.elementType

    new KGrid(t, dims, Range(0, dims.getNumberOfItems)
      .map(_ => {
        val v = et.readFromBinaryStream(input, tt)
        v
      })
      .toArray)
  }
}


class KGrid private (
  private val t: KGridType,
  val dims: KGrid.Dims,
  private val elements: Array[KValue])
  extends KValue
{
  import KGrid._

  if(dims.getRank != t.rank) {
    throw new IllegalArgumentException(
      "Number of dimensions should be equal to rank of grid type. Expected: "
        + t.rank + ", Actual: " + dims.getRank)
  }

  def this(t: KGridType, dims: KGrid.Dims) = this(t, dims,
    Range(0, dims.getNumberOfItems)
      .map(_ => t.elementType.getDefaultValue)
      .toArray)

  def this(t: KGridType, dims: Int*) = this(t, new KGrid.Dims(dims:_*))

  def get(index: Int*): KValue = elements(dims.ordinalForIndex(index))

  def set(r: KValue, index: Int*): KGrid = {
    if(r.getType != t.elementType) {
      throw new IllegalArgumentException("Type mismatch for value set at index "
        + indexToString(index) + ". Expected: " + t.elementType
        + ", Actual: " + r.getType)
    }
    val o = dims.ordinalForIndex(index)
    elements(o) = r
    this
  }

  override def getType: KType[_ <: KValue] = t

  override def getTotalSizeInOctets: Long = HEADER_SIZE +
    dims.getRank * KType.INTEGER.sizeInOctets +
    elements.map(_.getTotalSizeInOctets).sum

  override def writeToBinaryStream(output: OutputStream): Unit = {
    output.write(dims.getRank)
    dims.foreach(i => NumberIO.writeInt(i, output))
    elements.foreach(v => v.writeToBinaryStream(output))
  }

  override def equals(obj: Any): Boolean = obj match {
    case g: KGrid => g.dims.equals(dims) && g.elements.sameElements(elements)
    case _ => false
  }

  override def toString: String =
    dims.toString() + elements.mkString("(", ", ", ")")
}
