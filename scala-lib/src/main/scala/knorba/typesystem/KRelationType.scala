package knorba.typesystem
import java.io.InputStream

class KRelationType(name: KString) extends KType[KNothing](0, name) {
  override def isCastableTo(that: KType[_]): Boolean = false
  override def isAutomaticCastableTo(that: KType[_]): Boolean = false
  override def isPrimitive: Boolean = false
  override def hasConstantSize: Boolean = false
  override def getDefaultValue: KNothing = KNothing.VALUE
  override def readFromBinaryStream(input: InputStream, tt: TypeTable): KNothing =
    KNothing.VALUE
  override def getCompatibleTypes: Set[Class[_]] = throw new NotImplementedError()
  override def valueOf(obj: Object): KNothing = throw new NotImplementedError()
}
