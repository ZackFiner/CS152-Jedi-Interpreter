package context

import scala.util.parsing.combinator._
import expression._
import value._

class Jedi3Parsers extends Jedi2Parsers {
  
  // assignment ::= identifier ~ "=" ~ expression
  def assignment: Parser[Assignment] = identifier ~ "=" ~ expression ^^ {
    case c ~"="~ d => Assignment(c, d)
  }
  // iteration ::= "while" ~ "(" ~ expression ~ ")" ~ expression
  def iteration: Parser[Iteration] = "while" ~ "(" ~> expression ~ ")" ~ expression ^^ {
    case condition ~ ")" ~ body => Iteration(condition, body)
  }
  // dereference ::= "[" ~ expression ~ "]"
  
  //the expression must be some kind of variable, it can be an identifier to a variable, or the variable itself
  def dereference: Parser[Expression] = "["~> expression <~"]" ^^ {
    e => FunCall(Identifier("dereference"), List(e))
  }
  
  def thunk: Parser[MakeThunk] = "freeze"~"("~>expression<~")" ^^ {
    case a => MakeThunk(a)
  }
  def text: Parser[MakeText] = "delay"~"("~>expression<~")" ^^ {
    case a => MakeText(a)
  }
  
  def continue: Parser[Continue] = "continue" ^^ {
    case a => Continue()
  }
  //we appear to be missing make-var and other commands
  override def expression: Parser[Expression] = declaration | conditional | iteration | disjunction | failure("Invalid expression")
  override def term: Parser[Expression]  = lambda | thunk | text | continue | funCall | block | assignment | dereference | literal | "("~>expression<~")"
}