package expression

import context._
import value._
case class Iteration(condition:Expression, body:Expression) extends SpecialForm {
  def execute(env:Environment) = {
    /*def helper(cont:Expression, lastExec:Value):Value = cont.execute(env) match {
      case c: Boole => if (c==Boole(true)) helper(condition, body.execute(env)) else lastExec
      case _ => throw new TypeException("Loops must take a boolean expression as their condition")
    }*/
    //the above implementation is tail recursive and checks that the type of the check parameter is boolean
    var body_v:Value = Notification.DONE//i specified type here to avoid implicit casting to Notification (we want this to be a Value)
    while(condition.execute(env).asInstanceOf[Boole] == Boole(true)) {
      try {body_v = body.execute(env)}
      catch{
        case x: ContinueException => Notification.DONE
      }
    }
    body_v
    
    //helper(condition, Notification.DONE)
  }
}