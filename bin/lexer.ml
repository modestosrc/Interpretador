type tokentype =
  | ILLEGAL
  | EOF
  (* Identifiers + literals *)
  | IDENT
  | INT
  (* Operators *)
  | ASSIGN
  | PLUS
  | MINUS
  | BANG
  | ASTERISK
  | SLASH
  | EQ
  | NOT_EQ
  (* Delimiters *)
  | LT 
  | GT
  | COMMA
  | SEMICOLON
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  (* Keywords *)
  | FUNCTION
  | LET
  | TRUE 
  | FALSE
  | IF 
  | ELSE
  | RETURN
;;

type token = {
    token_type: tokentype;
    literal : string
};;

let tokentype_as_string = function
    | ILLEGAL -> "ILLEGAL"
    | EOF -> "EOF"
    | IDENT -> "IDENT"
    | INT -> "INT"
    | ASSIGN -> "ASSIGN"
    | PLUS -> "PLUS"
    | MINUS -> "MINUS"
    | BANG -> "BANG"
    | ASTERISK -> "ASTERISK"
    | SLASH -> "SLASH"
    | EQ -> "EQ"
    | NOT_EQ -> "NOT_EQ"
    | LT -> "LT"
    | GT -> "GT"
    | COMMA -> "COMMA"
    | SEMICOLON -> "SEMICOLON"
    | LPAREN -> "LPAREN"
    | RPAREN -> "RPAREN"
    | LBRACE -> "LBRACE"
    | RBRACE -> "RBRACE"
    | FUNCTION -> "FUNCTION"
    | LET -> "LET"
    | TRUE -> "TRUE"
    | FALSE -> "FALSE"
    | IF -> "IF"
    | ELSE -> "ELSE"
    | RETURN -> "RETURN"
;;

let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false;;

let is_digit = function '0' .. '9' -> true | _ -> false;;

class lexer input = object (self)
    val instr : string = input;
    val mutable position : int = -1;
    val mutable read_position : int = 0;
    val mutable ch : char option = None;

    method read_char = 
        if read_position >= String.length instr then (
            ch <- None)
        else (
            ch <- Some instr.[read_position]);
        position <- read_position;
        read_position <- read_position +1

    method peek_char =
        if read_position >= String.length instr then
            '\r'
        else
            instr.[read_position]

    (*Retorna um token*)
    method next_token =
        let new_token ttype liter =
            {token_type = ttype; literal = String.make 1 liter} 
        in


        let ident_token c = 
            let rec aux acm =
                let result_string = String.make 1 acm in
                let next_c = self#peek_char in
                if is_alpha next_c then (
                    self#read_char;
                    result_string ^ (aux next_c))
                else
                    result_string
            in
            let toklit = aux c in
            let keywords = [
                ("let", LET);
                ("fn", FUNCTION);
                ("true", TRUE);
                ("false", FALSE);
                ("if", IF);
                ("else", ELSE);
                ("return", RETURN);
                ] in
            let toktyp = ref IDENT in
            List.iter (fun (x, y) -> 
                if String.equal toklit x then toktyp := y) keywords;
            {token_type = !toktyp; literal = toklit}
        in

        let int_token c = 
            let rec aux acm =
                let x = self#peek_char in
                if is_digit x then (
                    self#read_char;
                    acm ^ aux (String.make 1 x))
                else
                    acm
            in
            let toklit = aux (String.make 1 c) in
            {token_type = INT; literal = toklit}
        in

        let make_token a = 
            match a with
            | ';' -> new_token SEMICOLON a
            | '(' -> new_token LPAREN a
            | ')' -> new_token RPAREN a
            | ',' -> new_token COMMA a
            | '+' -> new_token PLUS a
            | '-' -> new_token MINUS a
            | '*' -> new_token ASTERISK a
            | '/' -> new_token SLASH a
            | '{' -> new_token LBRACE a
            | '}' -> new_token RBRACE a
            | '<' -> new_token LT a
            | '>' -> new_token GT a
            | '=' -> if self#peek_char == '=' then (
                        self#read_char;
                        {token_type = EQ; literal = "=="})
                    else
                        new_token ASSIGN a
            | '!' -> if self#peek_char == '=' then (
                        self#read_char;
                        {token_type = NOT_EQ; literal = "!="})
                     else
                        new_token BANG a
            |  x  -> if is_alpha x then (
                        ident_token x)
                    else if is_digit x then (
                        int_token x)
                     else (
                        new_token ILLEGAL a) in

        let rec doit = function
            | Some ' ' -> self#read_char; doit ch
            | Some '\n' -> self#read_char; doit ch
            | Some '\r' -> self#read_char; doit ch
            | Some c    -> make_token c
            | None      -> new_token EOF ' ' 
        in

        self#read_char;
        doit ch
end

let repl = 
    while true do
        print_string ">>> ";
        let lex = new lexer @@ read_line () in
        let rec aux = function
            | {token_type = EOF; _ } -> ()
            | x -> 
                Printf.printf "{Token type: %s, Token literal: %s}\n"
                    (tokentype_as_string x.token_type) x.literal;
                aux lex#next_token
        in
        aux lex#next_token
    done
;;


let () =
    repl
;;
