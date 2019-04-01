package expression

import context.Environment
import value._

case class Identifier(val name: String) extends Expression {
  override def toString = name
  def execute(env: Environment) = env.apply(this) match {//we use apply now because we have blocks (thus we may need to search sub environments)
    case c:Thunk => c.thunk//handle thunks
    case c:Text => c.body.execute(env)//handle text
    case c => c//handle everything else
  }
  //override def hashCode = name.## //identifier's will be unique
}