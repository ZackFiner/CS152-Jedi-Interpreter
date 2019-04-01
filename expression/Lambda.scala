package expression
import context._
import value._
case class Lambda(params: List[Identifier], body: Expression) extends SpecialForm {
  def execute(env: Environment) = Closure(env, body, params)
}