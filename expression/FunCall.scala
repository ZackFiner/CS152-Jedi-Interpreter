package expression
import context._
import value._

case class FunCall(name:Identifier, operands:List[Expression]) extends Expression {
  def execute(env: Environment) = {
    /*
     * We need to figure out a way to evaluate closures, a closure is a value, but it
     * requires a set of arguments (values) in order to execute.
     */
    val args1 = operands.map(_.execute(env))//generate the values
    if (env.contains(name)) name.execute(env) match {
     	case c: Closure => c.apply(args1)
    	case default => throw new TypeException("You cannot execute on non-function type")
    } else alu.execute(name, args1)
    
  }
}