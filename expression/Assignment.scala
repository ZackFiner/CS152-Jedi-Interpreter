package expression
import context._
import value._
case class Assignment(vbl:Identifier, update:Expression) extends SpecialForm{
  def execute(env:Environment) = vbl.execute(env) match {//get the variable we need to update
    case c: Variable => {c.reassign(update.execute(env)); Notification.DONE}
    //case c: Variable => {val v = update.execute(env); c.reassign(v); v}//this allows for syntax like if (v=true == true) but doesn't return done
    case _ => Notification.UNSPECIFIED
  }
}