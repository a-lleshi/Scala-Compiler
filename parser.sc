// A parser for the WHILE language

// load the lexer
import $file.lexer, lexer._ 

// load the parser
import $file.token, token._

// more convenience for the map parsers later on;
// it allows writing nested patterns as
// case x ~ y ~ z => ...

case class ~[+A, +B](x: A, y: B)

// constraint for the input
type IsSeq[A] = A => Seq[_]


abstract class Parser[I : IsSeq, T]{
  def parse(in: I): Set[(T, I)]

  def parse_all(in: I) : Set[T] =
    for ((hd, tl) <- parse(in); 
        if tl.isEmpty) yield hd
}

// parser combinators

// sequence parser
class SeqParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 q: => Parser[I, S]) extends Parser[I, ~[T, S]] {
  def parse(in: I) = 
    for ((hd1, tl1) <- p.parse(in); 
         (hd2, tl2) <- q.parse(tl1)) yield (new ~(hd1, hd2), tl2)
}

// alternative parser
class AltParser[I : IsSeq, T](p: => Parser[I, T], 
                              q: => Parser[I, T]) extends Parser[I, T] {
  def parse(in: I) = p.parse(in) ++ q.parse(in)   
}

// map parser
class MapParser[I : IsSeq, T, S](p: => Parser[I, T], 
                                 f: T => S) extends Parser[I, S] {
  def parse(in: I) = for ((hd, tl) <- p.parse(in)) yield (f(hd), tl)
}



// atomic parser for Tokens from token.scala 
case class TokParser(t: List[Token]) extends Parser[List[Token], Token] {
  override def parse(in: List[Token]) = in match {
    case head :: tail => if (head == t.head) Set((head, tail)) else Set()
    case _ => Set()
  }
}

// Parser for WHILE language

case object IdentifierParser extends Parser[List[Token], String] {
    def parse(in:List[Token]) = in.head match {
      case T_ID(s:String) => Set((s,in.tail))
      case _ => Set()
    }
}

case object NumberParser extends Parser[List[Token], Int] {
  def parse(in:List[Token]) =  in.head match {
    case T_NUM(n:Int) => Set((n,in.tail))
    case _ => Set()
  }
}

case object StringParser extends Parser[List[Token], String] {
  def parse(in:List[Token]) = in.head match {
    case T_STR(s:String) => Set((s,in.tail))
    case _ => Set()
  }
}

// the following string interpolation allows us to write 
// StrParser(_some_string_) more conveniently as 
//
// p"<_some_string_>" 

// implicit def parser_interpolation(sc: StringContext) = new {
//     def p(args: Any*) = StrParser(sc.s(args:_*))
// }    

// more convenient syntax for parser combinators
implicit def ParserOps[I : IsSeq, T](p: Parser[I, T]) = new {
  def ||(q : => Parser[I, T]) = new AltParser[I, T](p, q)
  def ~[S] (q : => Parser[I, S]) = new SeqParser[I, T, S](p, q)
  def map[S](f: => T => S) = new MapParser[I, T, S](p, f)
}



// the abstract syntax trees for the WHILE language
abstract class Stmt
abstract class AExp
abstract class BExp 

type Block = List[Stmt]

case object Skip extends Stmt
case class If(a: BExp, bl1: Block, bl2: Block) extends Stmt
case class While(b: BExp, bl: Block) extends Stmt
case class For(s: String, a1: AExp, a2: AExp, bl: Block) extends Stmt
case class Assign(s: String, a: AExp) extends Stmt
case class Write(s: String) extends Stmt
case class Read(s: String) extends Stmt
case class WriteVar(s: String) extends Stmt
case class WriteStr(s: String) extends Stmt

case class Var(s: String) extends AExp
case class Num(i: Int) extends AExp
case class Aop(o: String, a1: AExp, a2: AExp) extends AExp

case object True extends BExp
case object False extends BExp
case class Bop(o: String, a1: AExp, a2: AExp) extends BExp
case class Lop(o: String, b1: AExp, b2: BExp) extends BExp
case class And(b1: BExp, b2: BExp) extends BExp
case class Or(b1: BExp, b2: BExp) extends BExp


// arithmetic expressions
lazy val AExp: Parser[List[Token], AExp] = 
  (Te ~ TokParser(List(T_OP("+"))) ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("+", x, z) } ||
  (Te ~ TokParser(List(T_OP("-"))) ~ AExp).map[AExp]{ case x ~ _ ~ z => Aop("-", x, z) } || Te

lazy val Te: Parser[List[Token], AExp] = 
  (Fa ~ TokParser(List(T_OP("*"))) ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("*", x, z) } ||
  (Fa ~ TokParser(List(T_OP("/"))) ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("/", x, z) } || 
  (Fa ~ TokParser(List(T_OP("%"))) ~ Te).map[AExp]{ case x ~ _ ~ z => Aop("%", x, z) } || Fa

lazy val Fa: Parser[List[Token], AExp] = 
   (TokParser(List(T_LPARA)) ~ AExp ~ TokParser(List(T_RPARA))).map{ case _ ~ y ~ _ => y } || 
   IdentifierParser.map(Var) || 
   NumberParser.map(Num) 

// boolean expressions with some simple nesting
lazy val BExp: Parser[List[Token], BExp] = 
  (AExp ~ TokParser(List(T_OP("=="))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("==", x, z) } ||
  (AExp ~ TokParser(List(T_OP("!="))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("!=", x, z) } ||
  (AExp ~ TokParser(List(T_OP("<"))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<", x, z) } ||
  (AExp ~ TokParser(List(T_OP(">"))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">", x, z) } ||
  (AExp ~ TokParser(List(T_OP("<="))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop("<=", x, z) } ||
  (AExp ~ TokParser(List(T_OP(">="))) ~ AExp).map[BExp]{ case x ~ _ ~ z => Bop(">=", x, z) } ||
  (TokParser(List(T_LPARA)) ~ BExp ~ TokParser(List(T_RPARA)) ~ TokParser(List(T_OP("&&"))) ~ BExp).map[BExp]{ case _ ~ x ~ _ ~ _ ~ z => And(x, z) } ||
  (TokParser(List(T_LPARA)) ~ BExp ~ TokParser(List(T_RPARA)) ~ TokParser(List(T_OP("||"))) ~ BExp).map[BExp]{ case _ ~ x ~ _ ~ _ ~ z => Or(x, z) } ||
  (TokParser(List(T_KWD("true"))).map[BExp]{ _ => True }) ||
  (TokParser(List(T_KWD("false"))).map[BExp]{ _ => False }) ||
  (TokParser(List(T_LPARA)) ~ BExp ~ TokParser(List(T_RPARA))).map[BExp]{ case _ ~ x ~ _ => x }

// a single statement 
lazy val Stmt: Parser[List[Token], Stmt] =
  ((TokParser(List(T_KWD("skip"))).map[Stmt]{_ => Skip }) ||
   (IdentifierParser ~ TokParser(List(T_OP(":="))) ~ AExp).map[Stmt]{ case x ~ _ ~ z => Assign(x, z) } ||
   (TokParser(List(T_KWD("write"))) ~ IdentifierParser).map[Stmt]{ case _ ~ y => WriteVar(y) } ||
   (TokParser(List(T_KWD("write"))) ~ StringParser).map[Stmt]{ case _ ~ y => WriteStr(y) } ||
   (TokParser(List(T_KWD("write"))) ~ TokParser(List(T_LPARA)) ~ IdentifierParser ~ TokParser(List(T_RPARA))).map[Stmt]{ case x ~ _ ~ y ~ _ => WriteVar(y) } ||
   (TokParser(List(T_KWD("write"))) ~ TokParser(List(T_LPARA)) ~ StringParser ~ TokParser(List(T_RPARA))).map[Stmt]{ case x ~ _ ~ y ~ _ => WriteStr(y) } ||
   (TokParser(List(T_KWD("read"))) ~ IdentifierParser).map[Stmt]{ case _ ~ y => Read(y) } ||
   (TokParser(List(T_KWD("read"))) ~ TokParser(List(T_LPARA)) ~ IdentifierParser ~ TokParser(List(T_RPARA))).map[Stmt]{ case x ~ _ ~ y ~ _ => Read(y) } ||
   (TokParser(List(T_KWD("if"))) ~ TokParser(List(T_LPARA)) ~ BExp ~ TokParser(List(T_RPARA)) ~ TokParser(List(T_KWD("then"))) ~ Block ~ TokParser(List(T_KWD("else"))) ~ Block)
     .map[Stmt]{ case _ ~ _ ~ y ~ _ ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
   (TokParser(List(T_KWD("if"))) ~ BExp ~ TokParser(List(T_KWD("then"))) ~ Block ~ TokParser(List(T_KWD("else"))) ~ Block)
     .map[Stmt]{ case _ ~ y ~ _ ~ u ~ _ ~ w => If(y, u, w) } ||
   (TokParser(List(T_KWD("while"))) ~ BExp ~ TokParser(List(T_KWD("do"))) ~ Block).map[Stmt]{ case _ ~ y ~ _ ~ w => While(y, w) } ||
   (TokParser(List(T_KWD("while"))) ~ TokParser(List(T_LPARA)) ~ BExp ~ TokParser(List(T_RPARA)) ~ TokParser(List(T_KWD("do"))) ~ Block).map[Stmt]{ case _ ~ _ ~ y ~ _ ~ _ ~ w => While(y, w) } ||
   (TokParser(List(T_KWD("for"))) ~ IdentifierParser ~ TokParser(List(T_OP(":="))) ~ AExp ~ TokParser(List(T_KWD("upto"))) ~ AExp ~ TokParser(List(T_KWD("do"))) ~ Block).map[Stmt]{ case _ ~ x ~ _ ~ y ~ _ ~ z ~ _ ~ w => For(x, y, z, w) } ||
   (TokParser(List(T_KWD("for"))) ~ TokParser(List(T_LPARA)) ~ IdentifierParser ~ TokParser(List(T_OP(":="))) ~ AExp ~ TokParser(List(T_RPARA)) ~ TokParser(List(T_KWD("upto"))) ~ TokParser(List(T_LPARA)) ~ AExp ~ TokParser(List(T_RPARA)) ~ TokParser(List(T_KWD("do"))) ~ Block).map[Stmt]{ case _ ~ _ ~ x ~ _ ~ y ~ _ ~ _ ~ _ ~ z ~ _ ~ _ ~ w => For(x, y, z, w) } 
   )


 
// statements
lazy val Stmts: Parser[List[Token], Block] =
  (Stmt ~ TokParser(List(T_SEMI)) ~ Stmts).map[Block]{ case x ~ _ ~ z => x :: z } ||
  (Stmt.map[Block]{ s => List(s) })

// blocks (enclosed in curly braces)
lazy val Block: Parser[List[Token], Block] =
  ((TokParser(List(T_LPARA)) ~ Stmts ~ TokParser(List(T_RPARA))).map{ case x ~ y ~ z => y } || 
   (Stmt.map(s => List(s))))


// an interpreter for the WHILE language
type Env = Map[String, Int]

def eval_aexp(a: AExp, env: Env) : Int = a match {
  case Num(i) => i
  case Var(s) => env(s)
  case Aop("+", a1, a2) => eval_aexp(a1, env) + eval_aexp(a2, env)
  case Aop("-", a1, a2) => eval_aexp(a1, env) - eval_aexp(a2, env)
  case Aop("*", a1, a2) => eval_aexp(a1, env) * eval_aexp(a2, env)
  case Aop("/", a1, a2) => eval_aexp(a1, env) / eval_aexp(a2, env)
  case Aop("%", a1, a2) => eval_aexp(a1, env) % eval_aexp(a2, env)
}

def eval_bexp(b: BExp, env: Env) : Boolean = b match {
  case True => true
  case False => false
  case Bop("==", a1, a2) => eval_aexp(a1, env) == eval_aexp(a2, env)
  case Bop("!=", a1, a2) => !(eval_aexp(a1, env) == eval_aexp(a2, env))
  case Bop(">", a1, a2) => eval_aexp(a1, env) > eval_aexp(a2, env)
  case Bop("<", a1, a2) => eval_aexp(a1, env) < eval_aexp(a2, env)
  case Bop(">=", a1, a2) => eval_aexp(a1, env) >= eval_aexp(a2, env)
  case Bop("<=", a1, a2) => eval_aexp(a1, env) <= eval_aexp(a2, env)
  case And(b1, b2) => eval_bexp(b1, env) && eval_bexp(b2, env)
  case Or(b1, b2) => eval_bexp(b1, env) || eval_bexp(b2, env)
}

def eval_stmt(s: Stmt, env: Env) : Env = s match {
  case Skip => env
  case Assign(x, a) => env + (x -> eval_aexp(a, env))
  case If(b, bl1, bl2) => if (eval_bexp(b, env)) eval_bl(bl1, env) else eval_bl(bl2, env) 
  case While(b, bl) => 
    if (eval_bexp(b, env)) eval_stmt(While(b, bl), eval_bl(bl, env))
    else env
  case For(x, a1, a2, bl) => 
    // convert for loop into while loop and evaluate
    val w = While(Bop("<=", Var(x), a2), bl :+ Assign(x, Aop("+", Var(x), Num(1))))
    eval_stmt(w, env + (x -> eval_aexp(a1, env)))
    
  case WriteVar(x) => { println(env(x)) ; env }
  case WriteStr(s) => { println(StringContext.processEscapes(s)) ; env  }
  case Read(x) => { println("Enter Num: "); val i = scala.io.StdIn.readInt() ; env + (x -> i) }
}

def eval_bl(bl: Block, env: Env) : Env = bl match {
  case Nil => env
  case s::bl => eval_bl(bl, eval_stmt(s, env))
}

def eval(bl: Block) : Env = eval_bl(bl, Map())


// helper code for timing information
def time_needed[T](n: Int, code: => T) = {
  val start = System.nanoTime()
  for (i <- 0 until n) code
  val end = System.nanoTime()
  (end - start)/(n * 1.0e9)
}


@main
def main(file: String) = {
  val prog0 = os.read(os.pwd / file)

  println(" ")

  val tokenised = tokenise( prog0 )
  val ast = Stmts.parse_all(tokenised)

  println("Filtered: \n")
  println(tokenised)

  println("\n AST: \n")
  println(ast)

  println("\n Result: \n")
  println(eval(ast.head))

}
