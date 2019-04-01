package value
import expression._
import context._
case class Thunk(thenv:Environment, thbody:Expression) extends Closure(thenv, thbody, Nil) {
  private var cache:Value = null
  def thunk = {if (cache == null) cache = super.apply(Nil); cache}//the closure will only be executed once, then the cache value will be returned whenever the thunk is called again
}