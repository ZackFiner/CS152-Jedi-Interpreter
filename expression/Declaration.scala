package expression
import context._
import value._
case class Declaration(name: Identifier, expression: Expression) extends SpecialForm {
  def execute(env:Environment) = {
    env.put(name, expression.execute(env))
    Notification.OK
  }
}