package expression
import context._
import value._
case class Conditional(condition: Expression, consequent:Expression, alternative:Expression=null) extends SpecialForm {
  def execute(env:Environment) = condition.execute(env) match {
    case Boole(true) => consequent.execute(env)
    case Boole(false) => if (alternative==null) Notification.UNSPECIFIED else alternative.execute(env)
    case default => throw new TypeException("Conditional statements must recieve Boole")
  }
}