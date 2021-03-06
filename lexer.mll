{
let reservedWords = [
  (* Keywords *)
  ("else", Parser.ELSE);
  ("false", Parser.FALSE);
  ("if", Parser.IF);
  ("then", Parser.THEN);
  ("true", Parser.TRUE);
  ("in", Parser.IN);
  ("let", Parser.LET);
  ("fun", Parser.FUN);
  ("dfun", Parser.DFUN);
  ("rec", Parser.REC);
  ("and", Parser.AND);
  ("match", Parser.MATCH);
  ("with", Parser.WITH);
] 
}

rule main = parse
  (* ignore spacing and newline characters *)
  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "(*" {comment lexbuf; main lexbuf}
| "-"? ['0'-'9']+
    { Parser.INTV (int_of_string (Lexing.lexeme lexbuf)) }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
| ";;" { Parser.SEMISEMI }
| "+" { Parser.PLUS }
| "*" { Parser.MULT }
| "<" { Parser.LT }
| "=" { Parser.EQ }
| "&&" { Parser.LAND }
| "||" { Parser.LOR }
| "->" { Parser.RARROW }
| "|" { Parser.PIPE }
| "[" { Parser.LBRACKET }
| "]" { Parser.RBRACKET }
| "::" { Parser.CONS }
| ";" { Parser.SEMI }
| ['a'-'z'] ['a'-'z' '0'-'9' '_' '\'']*
    { let id = Lexing.lexeme lexbuf in
      try 
        List.assoc id reservedWords
      with
      _ -> Parser.ID id
     }
| eof { exit 0 }

and comment = parse
  "(*" {comment lexbuf;comment lexbuf}
| "*)" {()}
| eof { exit 1 }
| _ {comment lexbuf}

