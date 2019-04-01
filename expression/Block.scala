package expression
import context._
import value._
case class Block(expressions: List[Expression]) extends SpecialForm{
  def execute(env:Environment): Value = {
    /*
    WARNING: I am not sure if .map executes in order; this could be a problem
    as the execution order must happen according to how they are inserted into
    the list.
    
    As part of the special form, there should be some kind of short cercuit here
     */
    val tempEnv = new Environment(env)//create a temporary environment (psuedo stack) for the block
    expressions.map(_.execute(tempEnv)).last//execute all expressions in the list (in them temp environment)
  }
  
}