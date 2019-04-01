package value

case class Variable(var content:Value) extends Value{
  def dereference() = content
  def reassign(newContent:Value) {content = newContent}//change the value held by this variable
  override def toString = "["+content+"]"
}