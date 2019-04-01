package expression
import context._
import value._
case class Disjunction(ops:List[Expression]) extends SpecialForm {
  def shortCircuit(env:Environment, ops:List[Expression]):Boole = {
    if (ops!=Nil) ops.head.execute(env) match {
      case Boole(s) => if (s) Boole(true) else shortCircuit(env, ops.tail)
      case default => throw new TypeException("Disjunction must take type Boole")
    } else Boole(false)
  }
  
  def execute(env:Environment):Value = if (ops!=Nil) shortCircuit(env, ops) else throw new Exception("Disjunction must have operand(s)")
}