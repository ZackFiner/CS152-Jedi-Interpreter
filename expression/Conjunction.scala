package expression
import context._
import value._
case class Conjunction(ops: List[Expression]) extends SpecialForm {
  def shortCircuit(env: Environment, ops: List[Expression]):Boole = {
    if (ops!= Nil) ops.head.execute(env) match {
      case Boole(s) => if (!s) Boole(false) else shortCircuit(env, ops.tail)
      case default => throw new TypeException("Conjunction must take Boole type")
    } else Boole(true)//we will be checking for nil ops at the execute level, so this short circuit will never happen if there is no operands
    
  }
  def execute(env: Environment) = if (ops!=Nil) shortCircuit(env, ops) else throw new Exception("Conjunction must have operand(s)")
}