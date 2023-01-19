// Tokenise a WHILE program into tokens created by Abdurrahman Lleshi


// load the lexer
import $file.lexer, lexer._ 


// The tokens for the WHILE language

abstract class Token 
case object T_SEMI extends Token
case object T_LPARA extends Token
case object T_RPARA extends Token
case class T_ID(s: String) extends Token
case class T_OP(s: String) extends Token
case class T_NUM(n: Int) extends Token
case class T_KWD(s: String) extends Token
case class T_STR(s: String) extends Token
case class T_LETTER(s: String) extends Token
case class T_SYM(s: String) extends Token

val token : PartialFunction[(String, String), Token] = {
  case ("s", _) => T_SEMI
  case ("p", "(") => T_LPARA
  case ("p", ")") => T_RPARA
  case ("p", "{") => T_LPARA
  case ("p", "}") => T_RPARA
  case ("i", s) => T_ID(s)
  case ("o", s) => T_OP(s)
  case ("n", s) => T_NUM(s.toInt)
  case ("k", s) => T_KWD(s)
  case ("str", s) => T_STR(s)
  case ("l", s) => T_LETTER(s)
  case ("sym", s) => T_SYM(s)
}

// by using collect we filter out all unwanted tokens
def tokenise(s: String) : List[Token] = 
  lexing_simp(WHILE_REGS, s).collect(token)

