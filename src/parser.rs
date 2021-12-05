use itertools::Itertools;
use litrs::{CharLit, StringLit};

pub use parser::program as parse_program;

#[derive(Debug, Clone)]
pub struct Program {
    statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    PatDef(PatDef),
    Expr(Expr),
}

#[derive(Debug, Clone)]
pub struct PatDef {
    pub name: String,
    pub matches: Vec<Match>,
}

#[derive(Debug, Clone)]
pub struct Match {
    binding: Binding,
    expr: Expr,
}

#[derive(Debug, Clone)]
pub enum Binding {
    Char(char),
    Concat(Box<Binding>, Box<Binding>),
    ListOf(Box<Binding>, Option<ListLenBinding>),
    Tuple(Vec<Binding>),
    Named(String, Box<Binding>),
    // TODO: ignoring types for now
    Type(String),
    // TODO: should this be an Expr and not only a Ref?
    Ref(String),
    Anything,
}

#[derive(Debug, Clone)]
pub enum ListLenBinding {
    Min(usize),
    ToName(String),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Comment(String),
    CharLiteral(char),
    StringLiteral(String),
    IntLiteral(i128),
    If(Box<Conditional>),
    While(Box<Conditional>),
    ListCompEl(String),
    ListCompIndex(String),
    Length(Box<Expr>),
    Ref(String),
    Block(Vec<Expr>),
    Assignment(String, Box<Expr>),
    ListComprehension { list: Box<Expr>, over: Box<Expr> },
    ListLiteral(Vec<Expr>),
    CallPat(Box<Expr>, Box<Expr>),
    Range(Box<Expr>, Box<Expr>),
    BinOp(Box<Expr>, Op, Box<Expr>),
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
    And,
    Or,
    Pow,
    Shl,
}

#[derive(Debug, Clone)]
pub struct Conditional {
    pub cond: Expr,
    pub then: Expr,
    pub r#else: Option<Expr>,
}

peg::parser! {
    pub grammar parser() for str {
        pub rule program() -> Program
            = statements:(statement_with_whitespace()+) { Program { statements } }

        rule statement_with_whitespace() -> Statement
            = _* statement:statement() _* { statement }
        rule statement() -> Statement
            = pat_def_statement() / expr_statement()


        rule comment_expr() -> Expr
            = comment:comment_string() { Expr::Comment(comment) }
        rule comment_string() -> String
            = "/" "/" onespace()? body:$([^ '\r' | '\n']*)? following:following_comment()*  {
                body.map(|b| b.to_owned()).into_iter().chain(following.into_iter()).join("\n")
            }
        rule following_comment() -> String
            = newline() c:comment_string() {
                if c.starts_with("//") {
                    let c = c.trim_start_matches("//");
                    let c = c.strip_prefix(' ').unwrap_or(c);
                    format!("\n{}", c)
                } else {
                    c
                }
            }

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
            = (named_binding() / char_binding() / tuple_binding() / binding_in_parens() / list_binding() /
               any_binding() / type_binding() / ref_binding())

        rule tuple_binding() -> Binding
            = "(" _? bindings:(binding() ** comma()) _? ")" {
                Binding::Tuple(bindings)
            }
        rule binding_in_parens() -> Binding
            = "(" _? binding:binding() _? ")" { binding }

        rule list_binding() -> Binding
            = "[" _? binding:binding() _? "]" _? len:list_len_binding()? {
                Binding::ListOf(Box::new(binding), len)
            }
        rule list_len_binding() -> ListLenBinding
            = "{" _? inner:list_len_binding_inner() _? "}" { inner }
        rule list_len_binding_inner() -> ListLenBinding
            = name:ident() { ListLenBinding::ToName(name.to_owned()) }
            / min:int() "+" { ListLenBinding::Min(min as _) }

        rule named_binding() -> Binding
            = name:ident() _? "@" _? binding:binding() {
                Binding::Named(name.to_owned(), Box::new(binding))
            }
        rule char_binding() -> Binding
            = char:char_lit() { Binding::Char(char) }
        rule any_binding() -> Binding
            = "ANY" { Binding::Anything }
        rule type_binding() -> Binding
            = name:type_ident() { Binding::Type(name.to_owned()) }
        rule ref_binding() -> Binding
            = name:ident() { Binding::Ref(name.to_owned()) }

        rule expr_statement() -> Statement = expr:expr() { Statement::Expr(expr) }

        rule expr() -> Expr
            = (comment_expr() / bin_op_expr() / range_expr() / assignment_expr() / list_comprehension_expr() /
               list_literal_expr() / string_literal_expr() / if_else_expr() / if_no_else_expr() /
               while_expr() / scalar_expr() / this_el_expr() / block_expr())

        rule bin_op_expr() -> Expr
            = left:scalar_expr() _? op:op() _? right:scalar_expr() {
                Expr::BinOp(Box::new(left), op, Box::new(right))
            }

        rule op() -> Op
            = ("+" { Op::Add } / "/" { Op::Div } / "-" { Op::Sub } / "*" { Op::Mul } /
               "**" { Op::Pow } / "==" { Op::Eq } / "!=" { Op::Neq } / ">" { Op::Gt } /
               "<<" { Op::Shl } / "<" { Op::Lt } / ">=" { Op::Gte } / "<=" { Op::Lte } /
               "&&" { Op::And } / "||" { Op::Or })

        rule range_expr() -> Expr
            = start:scalar_expr() _? ".." _? end:scalar_expr() {
                Expr::Range(Box::new(start), Box::new(end))
            }

        rule scalar_expr() -> Expr
            = (parens_expr() / call_pat_expr() / char_literal_expr() / int_literal_expr() / this_el_expr() / index_expr() /
               length_expr() / ref_expr())

        rule parens_expr() -> Expr
            = "(" _? expr:expr() _? ")" { expr }

        // TODO: can we () call any expr instead of only names?
        rule call_pat_expr() -> Expr
            = name:ident() _? arg:expr() {
                Expr::CallPat(Box::new(Expr::Ref(name.to_owned())), Box::new(arg))
            }

        rule list_comprehension_expr() -> Expr
            = "[" _? list:expr() _? "<-" _? over:expr() _? "]" {
                Expr::ListComprehension { list: Box::new(list), over: Box::new(over) }
            }

        rule list_literal_expr() -> Expr
            = "[" _? exprs:(expr() ** comma()) _? "]" { Expr::ListLiteral(exprs) }
        rule assignment_expr() -> Expr
            = name:ident() _? "=" _? expr:expr() { Expr::Assignment(name.to_owned(), Box::new(expr)) }

        rule block_expr() -> Expr
            = "{" _? exprs:(expr() ** whitespace()) _? "}" { Expr::Block(exprs) }

        rule while_expr() -> Expr
            = "while" _ cond:expr() _? then:expr() {
                Expr::While(Box::new(Conditional { cond, then, r#else: None }))
            }

        rule if_else_expr() -> Expr
            = "if" _ cond:expr() _? then:expr() _? r#else:expr() {
                Expr::If(Box::new(Conditional { cond, then, r#else: Some(r#else) }))
            }
        rule if_no_else_expr() -> Expr
            = "if" _ cond:expr() _? then:expr() {
                Expr::If(Box::new(Conditional { cond, then, r#else: None }))
            }

        rule this_el_expr() -> Expr
            = "*" ident:ident() { Expr::ListCompEl(ident.to_owned()) }
        rule index_expr() -> Expr
            = "%" ident:ident() { Expr::ListCompIndex(ident.to_owned()) }
        rule length_expr() -> Expr
            = "#" expr:expr() { Expr::Length(Box::new(expr)) }
        rule ref_expr() -> Expr
            = name:ident() { Expr::Ref(name.to_owned()) }
        rule char_literal_expr() -> Expr
            = char_lit:char_lit() { Expr::CharLiteral(char_lit) }
        rule string_literal_expr() -> Expr
            = string_lit:string_lit() { Expr::StringLiteral(string_lit) }
        rule int_literal_expr() -> Expr = int:int() { Expr::IntLiteral(int) }

        rule string_lit() -> String
            = str:$("\"" (!['"'][_] / "\"\"")* "\"") {?
                Ok(StringLit::parse(str).or_else(|e| { dbg!(str, e) ; Err("string_lit: " ) })?.value().to_owned())
            }
        rule char_lit() -> char
            = char:$("'" "\\"? [_] "'") {?
                Ok(CharLit::parse(char).or_else(|e| { dbg!(e) ; Err("char_lit: " ) })?.value())
            }
        rule int() -> i128
            = int:$("0" / "-"? ['1' ..= '9']+ ['0' ..= '9']*) {? int.parse().or(Err("not a number")) }
        rule type_ident() -> &'input str = $(type_ident_start()+ ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
        rule type_ident_start() -> &'input str = $(['A'..='Z' | '_']+)
        rule ident() -> &'input str = $(ident_start()+ ['a'..='z' | 'A'..='Z' | '_' | '0'..='9']*)
        rule ident_start() -> &'input str = $(['a'..='z' | '_']+)
        rule comma() -> () = _? "," _?
        rule nbspace() = onespace()+
        rule onespace() = [' ' | '\t']
        rule newline() = "\n" / "\r\n"
        rule whitespace() = (nbspace() / newline())+
        rule _() = quiet!{ whitespace() }
    }
}
