package context


  
import expression._
import value._

/*
 * Notes:
 * alu implements all low-level arithmetic, logic, and I/O functions
 * alu does lots of type checking
 * alu is a singleton
 */
object alu {
  // dispatcher
  def execute(opcode: Identifier, args: List[Value]): Value = {
    opcode.name match {
      case "add" => add(args)
      case "mul" => mul(args)
      case "sub" => sub(args)
      case "div" => div(args)
      case "less" => less(args)
      case "more" => more(args)
      case "equals" => equals(args)
      case "unequals" => unequals(args)
      case "not" => not(args)
      // variables
      case "dereference" => dereference(args)
      case "var" => makeVar(args)
      // primitive I/O ops:
      case "write" => write(args)
      case "prompt" => prompt(args)
      case "read" => read(args)
      // store ops
      case "store" => store(args)
      case "put" => put(args)
      case "rem" => rem(args)
      case "contains" => contains(args)
      case "map" => map(args)
      case "filter" => filter(args)
      case "get" => get(args)
      case "addLast" => addLast(args)
      case "size" => size(args)
      case _ => throw new UndefinedException(opcode)
    }
  }
  
    private def toInt(arg: Value): Option[Integer] =
      if (arg.isInstanceOf[Integer]) Some(arg.asInstanceOf[Integer]) else None
      
    private def toReal(arg: Value): Option[Real] =
      if (arg.isInstanceOf[Real]) Some(arg.asInstanceOf[Real]) 
      else if (arg.isInstanceOf[Integer]) Some(Integer.intToReal(arg.asInstanceOf[Integer]))
      else None
      
    private def toChars(arg: Value): Option[Chars] =
      if (arg.isInstanceOf[Chars]) Some(arg.asInstanceOf[Chars]) else None
      
    private def toBoole(arg: Value): Option[Boole] =
      if (arg.isInstanceOf[Boole]) Some(arg.asInstanceOf[Boole]) else None
      
    private def add(args: List[Value]) = {
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) args2.flatten.reduce(_+_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_+_)
        else {
          val args4 = args.map(toChars).filter(_ != None)
          if (args4.size == args.size) args4.flatten.reduce(_+_)
          else {
            throw new TypeException("Inputs to + must be numbers or texts")
          }
        }
      }
    }
    private def sub(args: List[Value]) = {
      val args2 = args.map(toInt).filter(_ != None)
      if (args2.size == args.size) args2.flatten.reduce(_-_)
      else {
        val args3 = args.map(toReal).filter(_ != None)
        if (args3.size == args.size) args3.flatten.reduce(_-_)
        else {
          throw new TypeException("Inputs to - must be numbers")
        }
      }
    }
    private def mul(args: List[Value]) = {
        val args2 = args.map(toInt).filter(_ != None)
        if (args2.size == args.size) args2.flatten.reduce(_*_)
        else {
          val args3 = args.map(toReal).filter(_ != None)
          if (args3.size == args.size) args3.flatten.reduce(_*_)
          else {
            throw new TypeException("Inputs to * must be numbers")
          }
        }
      }
  
    private def div(args: List[Value]) = {
        val args2 = args.map(toInt).filter(_ != None)
        if (args2.size == args.size) args2.flatten.reduce(_/_)
        else {
          val args3 = args.map(toReal).filter(_ != None)
          if (args3.size == args.size) args3.flatten.reduce(_/_)
          else {
            throw new TypeException("Inputs to / must be numbers")
          }
        }
    }
    def less(args: List[Value]): Value = {
        if (args.length  != 2) throw new TypeException("less expects two inputs")
        val args2 = args.map(toInt).filter(_ != None)
        if (args2.size == args.size) Boole(args2(0) < args2(1))
        else {
          val args3 = args.map(toReal).filter(_ != None)
          if (args3.size == args.size) Boole(args3(0) < args3(1))
          else {
            val args4 = args.map(toChars).filter(_ != None)
            if (args4.size == args.size) Boole(args4(0) < args4(1))
            else throw new TypeException("Inputs to < must be numbers or texts")
          }
        }
     }  
      def more(args: List[Value]): Value = {
        if (args.length  != 2) throw new TypeException("more expects two inputs")
        val args2 = args.map(toInt).filter(_ != None)
        if (args2.size == args.size) Boole(args2(0) > args2(1))
        else {
          val args3 = args.map(toReal).filter(_ != None)
          if (args3.size == args.size) Boole(args3(0) > args3(1))
          else {
            val args4 = args.map(toChars).filter(_ != None)
            if (args4.size == args.size) Boole(args4(0) > args4(1))
            else throw new TypeException("Inputs to > must be numbers or texts")
          }
        }
      }
      /*
       * Grader has specified the following evaluations are wrong:
       * 3 == true -> type error
			 * 3 == 2 + 1 == 9 / 3 ->type error
			 * As such, we can conclude that equals should not be strictly typed (rather, it can accept multiple types but return false),
			 * to evaluate multiple operands
       */
     def equals(args: List[Value]): Value = {
       if (args.size < 2)
         throw new TypeException("equals expects at least two inputs")
       def compareElms(op1:Value, op2:Value):Boole = {
         val args = List(op1, op2)
         val args2 = args.map(toInt).filter(_ != None)
         if (args2.size == args.size) Boole(args2(0) == args2(1))
         else {
           val args3 = args.map(toReal).filter(_ != None)
           if (args3.size == args.size) Boole(args3(0) == args3(1))
           else {
             val args4 = args.map(toChars).filter(_ != None)
             if (args4.size == args.size) Boole(args4(0) == args4(1))
             else {
               val args5 = args.map(toBoole).filter(_ != None)
               if (args5.size == args.size) Boole(args5(0) == args5(0))
               else Boole(false);
             }
           }
         }
       }
       def helper(args: List[Value],lastVal:Value, lastCheck:Boole): Boole = if (args==Nil) lastCheck else helper(args.tail, args.head, lastCheck && compareElms(lastVal, args.head))
       helper(args.tail, args.head, Boole(true))
      }
      def not(args: List[Value]): Value = {
        if (args.length  != 1) throw new TypeException("not expects one input")
        val args2 = args.map(toBoole).filter(_ != None)
        if (args2.size == args.size) !(args2(0).head)
        else throw new TypeException("Inputs to ! must be a boolean")
      }
      def unequals(args: List[Value]): Value = not(List(equals(args)))
   
      def write(vals: List[Value]): Value = { println(vals(0)); Notification.DONE }
      def read(vals: List[Value]): Value = { val result = io.StdIn.readDouble(); Real(result)}
      def prompt(vals: List[Value]): Value = { print("=> "); Notification.DONE }

   // variable ops
   
   // returns the content of args(0)
   private def dereference(args: List[Value]) = if (args.size == 1) args(0) match {
     case c: Variable => c.dereference()
     case _ => throw new TypeException("You can only de-reference variable types")
   } else throw new TypeException("dereference expects 1 argument")
   
   // creates a new variable containing args(0)
   private def makeVar(args: List[Value]) = if (args.size > 0) Variable(args(0)) else throw new TypeException("variables must be initialized to some value; 1 argument missing")
   
   // store ops
   import collection.mutable._
   // returns a new store containing args
   private def store(args: List[Value]) = Store(args.to[ArrayBuffer])
   
   // put(v: Value, p: Integer, s: Store) calls s.put(v, p)
   private def put(args: List[Value]) = {
     if (args.size != 3)
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     if(!args(1).isInstanceOf[Integer] || !args(2).isInstanceOf[Store]) 
        throw new TypeException("expected signature: put(v: Value, p: Integer, s: Store)")
     args(2).asInstanceOf[Store].put(args(0), args(1).asInstanceOf[Integer])
     Notification.DONE
   } 
   
   // rem(p: Integer, s: Store) calls s.rem(p)
   private def rem(args: List[Value]) = if (args.size == 2) {
       if (args(0).isInstanceOf[Integer]) {
         if (args(1).isInstanceOf[Store]) {
           args(1).asInstanceOf[Store].rem(args(0).asInstanceOf[Integer])
         } else throw new TypeException("second argument must be Store, found:" + args(1).getClass())
       } else throw new TypeException("first argument must be Integer, found:" + args(0).getClass())
   } else throw new TypeException("remove expects 2 arguments")
       
   
   // get(p: Integer, s: Store) calls s.get(p)
   private def get(args: List[Value]) = if (args.size == 2) {
       if (args(0).isInstanceOf[Integer]) {
         if (args(1).isInstanceOf[Store]) {
           args(1).asInstanceOf[Store].get(args(0).asInstanceOf[Integer])
         } else throw new TypeException("second argument must be Store, found:" + args(1).getClass())
       } else throw new TypeException("first argument must be Integer, found:" + args(0).getClass())
   } else throw new TypeException("get expects 2 arguments")
   
   // map(f: Closure, s: Store) calls s.map(f)
   private def map(args: List[Value]) = if (args.size == 2) {
       if (args(0).isInstanceOf[Closure]) {
         if (args(1).isInstanceOf[Store]) {
           args(1).asInstanceOf[Store].map(args(0).asInstanceOf[Closure])
         } else throw new TypeException("second argument must be Store, found:" + args(1).getClass())
       } else throw new TypeException("first argument must be Closure, found:" + args(0).getClass())
   } else throw new TypeException("map expects 2 arguments")
   
   // filter(f: Closure, s: Store) calls s.filter(f)
   private def filter(args: List[Value]) = if (args.size == 2) {
       if (args(0).isInstanceOf[Closure]) {
         if (args(1).isInstanceOf[Store]) {
           args(1).asInstanceOf[Store].filter(args(0).asInstanceOf[Closure])
         } else throw new TypeException("second argument must be Store, found:" + args(1).getClass())
       } else throw new TypeException("first argument must be Closure, found:" + args(0).getClass())
   } else throw new TypeException("filter expects 2 arguments")
   
   // contains(v: Value, s: Store) calls s.contains(v)
   private def contains(args: List[Value]) = if (args.size == 2) {
      if (args(1).isInstanceOf[Store]) {
         args(1).asInstanceOf[Store].contains(args(0))
      } else throw new TypeException("second argument must be Store, found:" + args(1).getClass())
   } else throw new TypeException("contains expects 2 arguments")
   
   // addLast(v: Value, s: Store) calls s.add(v)
   private def addLast(args: List[Value]) = if (args.size == 2) {
      if (args(1).isInstanceOf[Store]) {
         args(1).asInstanceOf[Store].add(args(0))
         Notification.DONE
      } else throw new TypeException("second argument must be Store, found:" + args(1).getClass())
   } else throw new TypeException("add expects 2 arguments")
   
   // size(s: Store) calls s.size
   private def size(args: List[Value]) = if (args.size == 1) {
      if (args(0).isInstanceOf[Store]) {
         args(0).asInstanceOf[Store].size
      } else throw new TypeException("first argument must be Store, found:" + args(0).getClass())
   } else throw new TypeException("add expects 1 argument")
   
  // etc.
}