package expression
import context._
import value._
case class MakeThunk(body:Expression) extends SpecialForm {
  def execute(env:Environment) = Thunk(env, body)
}