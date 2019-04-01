package value
import expression._
import context._
/*
 * Due to the fact that closure has thunk inherit from it, i have made this a regular class with a companion object instead
 * of a case class.
 */
class Closure(val defEnv: Environment, val body: Expression, val params:List[Identifier]) extends Value {
  def apply(args: List[Value]) = {
    if (args.size == params.size) {
      val tempEnv = new Environment(defEnv)//create temp environment in defining environment
      tempEnv.bulkPut(params, args)//this is why this method is useful
      body.execute(tempEnv)//return the value of the expression
    }
    else
    {
      Notification.UNSPECIFIED//might want to throw a syntax exception here
    }
  }
}

object Closure {
  def apply(env:Environment, main:Expression, para:List[Identifier]) = new Closure(env, main, para)
}