package expression
import context._
import value._
case class MakeText(body:Expression) extends SpecialForm{
  def execute(env:Environment) = Text(body)
}