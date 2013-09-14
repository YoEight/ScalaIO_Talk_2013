package net.deiko.lalr

trait ToBytes[A] {
  def toBytes(v: A): Array[Byte]
}

object ToBytes {
  implicit val stringToBytes = new ToBytes[String] {
    def toBytes(v: String) = v.getBytes("UTF-8")
  }
}
