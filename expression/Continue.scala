package expression

import context._
import value._
case class Continue() extends SpecialForm {
  def execute(env: Environment):Value = throw new ContinueException()
}