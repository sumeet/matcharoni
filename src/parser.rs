use litrs::CharLit;
use std::ops::Range;

pub use parser::program as parse_program;

#[derive(Debug)]
pub struct Program {
    statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    PatDef(PatDef),
    Expr(Expr),
}

#[derive(Debug)]
pub struct PatDef {
    name: String,
    matches: Vec<Match>,
}

#[derive(Debug)]
pub struct Match {
    binding: Binding,
    expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Char(char),
    Concat(Box<Binding>, Box<Binding>),
    ListOf(Box<Binding>, Option<ListLenBinding>),
    Named(String, Box<Binding>),
}

#[derive(Debug, Clone)]
pub enum ListLenBinding {
    Min(usize),
    ToName(String),
}

#[derive(Debug)]
pub enum Expr {
    CharLiteral(char),
    IntLiteral(i128),
    If(Box<Conditional>),
    While(Box<Conditional>),
    Ref(String),
}

#[derive(Debug)]
pub struct Conditional {
    cond: Expr,
    then: Expr,
    r#else: Option<Expr>,
}

peg::parser! {
    pub grammar parser() for str {
        pub rule program() -> Program
            = statements:(statement_with_whitespace()+) { Program { statements } }

        rule statement_with_whitespace() -> Statement
            = _* statement:statement() _* { statement }
        rule statement() -> Statement
            = pat_def_statement() / expr_statement()

        rule pat_def_statement() -> Statement
            = pat_def:pat_def() { Statement::PatDef(pat_def) }

        rule pat_def() -> PatDef
            = "pat" _? name:ident() _? "{" _ matches:(match() ** comma()) _? "}" {
                PatDef { name: name.to_owned(), matches }
            }

        rule match() -> Match
            = binding:binding() _? "=>" _? expr:expr() { Match { binding, expr } }

        rule binding() -> Binding
            = concat_binding() / scalar_binding()

        rule concat_binding() -> Binding
            = binding1:scalar_binding() _? "~" _? binding2:scalar_binding() {
                Binding::Concat(Box::new(binding1), Box::new(binding2))
            }
        rule scalar_binding() -> Binding
            = named_binding() / char_binding() // list_binding()
        rule named_binding() -> Binding
            = name:ident() _? "@" _? "(" _? binding:binding() _? ")" {
                Binding::Named(name.to_owned(), Box::new(binding))
            }
        rule char_binding() -> Binding
            = char:char_lit() { Binding::Char(char) }

        rule expr_statement() -> Statement = expr:expr() { Statement::Expr(expr) }
        rule expr() -> Expr
            = char_literal_expr() / int_literal_expr() / if_else_expr() / if_no_else_expr() / while_expr() / ref_expr()

        rule while_expr() -> Expr
            = "while" _ cond:expr() _? ":" _? then:expr() {
                Expr::While(Box::new(Conditional { cond, then, r#else: None }))
            }

        rule if_else_expr() -> Expr
            = "if" _ cond:expr() _? ":" _? then:expr() _ "else:" _? r#else:expr() {
                Expr::If(Box::new(Conditional { cond, then, r#else: Some(r#else) }))
            }
        rule if_no_else_expr() -> Expr
            = "if" _ cond:expr() _? ":" _? then:expr() {
                Expr::If(Box::new(Conditional { cond, then, r#else: None }))
            }

        rule ref_expr() -> Expr
            = name:ident() { Expr::Ref(name.to_owned()) }
        rule char_literal_expr() -> Expr
            = char:char_lit() { Expr::CharLiteral(char) }
        rule int_literal_expr() -> Expr = int:int() { Expr::IntLiteral(int) }

        rule char_lit() -> char
            = char:$("'" "\\"? [_] "'") {?
                Ok(CharLit::parse(char).or_else(|e| { dbg!(e) ; Err("char_lit: " ) })?.value())
            }
        rule int() -> i128
            = int:$("0" / "-"? ['1' ..= '9']+ ['0' ..= '9']*) {? int.parse().or(Err("not a number")) }
        rule ident() -> &'input str = $(ident_start()+ ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
        rule ident_start() -> &'input str = $(['a'..='z' | 'A'..='Z' | '_']+)
        rule comma() -> () = _? "," _?
        rule nbspace() = onespace()+
        rule onespace() = [' ' | '\t']
        rule newline() = "\n" / "\r\n"
        rule whitespace() = (nbspace() / newline())+
        rule _() = quiet!{ whitespace() }
    }
}
