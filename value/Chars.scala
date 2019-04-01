package value

import expression.Literal

case class Chars(val value:String) extends Literal with Ordered[Chars] with Equals {
  override def canEqual(other: Any) = other.isInstanceOf[Chars]
  override def equals(other: Any): Boolean = 
     other match {
      case other: Chars => this.canEqual(other) && (other.value == this.value)
      case _ => false
  }
  override def hashCode = this.toString.##
  override def toString = value.toString
  def compare(other: Chars) = this.value.compare(other.value)
  override def <(other: Chars) = (this.value < other.value) == true
  def ==(other: Chars) = Boole(this.value == other.value)
  def substring(n1: Integer, n2: Integer) = Chars(this.value.substring(n1.value, n2.value))
  def +(other: Chars) = Chars(this.value + other.value)
}