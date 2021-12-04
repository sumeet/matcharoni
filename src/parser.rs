use litrs::CharLit;

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

#[derive(Debug)]
pub enum Binding {
    Expr,
}

#[derive(Debug)]
pub enum Expr {
    CharLiteral(char),
    IntLiteral(i128),
    If(Box<Conditional>),
    While(Box<Conditional>),
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
            = expr_statement()
        rule expr_statement() -> Statement = expr:expr() { Statement::Expr(expr) }

        rule expr() -> Expr
            = char_literal() / int_literal() / if_else_expr() / if_no_else_expr() / while_expr()

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

        rule char_literal() -> Expr
            = char:$("'" "\\"? [_] "'") {?
                let ch = CharLit::parse(char).or_else(|e| { dbg!(e) ; Err("char_literal: " ) })?;
                Ok(Expr::CharLiteral(ch.value()))
            }
        rule int_literal() -> Expr = int:int() { Expr::IntLiteral(int) }

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
