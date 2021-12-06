use crate::parser;
use crate::parser::{Binding, Conditional, Expr, ListLenBinding, Op, Statement};
use anyhow::bail;
use std::collections::HashMap;

enum Scope {
    // should values be Rcd / Gcd?
    Block(HashMap<String, Value>),
    ListComp {
        name: Option<String>,
        index: usize,
        list: Vec<Value>,
    },
}

impl Scope {
    fn new_block_scope() -> Self {
        Scope::Block(HashMap::new())
    }

    fn set(&mut self, name: String, value: Value) -> anyhow::Result<()> {
        match self {
            Scope::Block(map) => {
                map.insert(name.clone(), value);
                Ok(())
            }
            Scope::ListComp { .. } => {
                anyhow::bail!("can't assign insided a list comprehension...yet...")
            }
        }
    }
}

struct Interpreter {
    scope: Vec<Scope>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            scope: vec![Scope::new_block_scope()],
        }
    }

    fn push_list_comp_scope(&mut self, name: Option<String>, list: Vec<Value>) {
        self.scope.push(Scope::ListComp {
            name,
            index: 0,
            list,
        });
    }

    fn incr_list_comp_index(&mut self) -> anyhow::Result<()> {
        let scope = self.scope.last_mut().unwrap();
        match scope {
            Scope::ListComp { index, .. } => Ok(*index += 1),
            _ => bail!("incr_list_comp_index called on non-list-comp scope"),
        }
    }

    fn push_block_scope(&mut self) {
        self.scope.push(Scope::new_block_scope());
    }

    fn pop_scope(&mut self) {
        self.scope.pop();
    }

    fn this_scope(&mut self) -> anyhow::Result<&mut Scope> {
        self.scope
            .last_mut()
            .ok_or_else(|| anyhow::anyhow!("No scope"))
    }

    fn eval_statement(&mut self, statement: &Statement) -> anyhow::Result<()> {
        match statement {
            Statement::PatDef(pat_def) => self.define_pattern(pat_def),
            Statement::Expr(expr) => {
                self.eval_expr(expr)?;
                Ok(())
            }
        }
    }

    fn define_pattern(&mut self, pat_def: &parser::PatDef) -> anyhow::Result<()> {
        self.this_scope()?.set(
            pat_def.name.clone(),
            Value::Pattern(Pattern(pat_def.clone())),
        )
    }

    fn eval_expr(&mut self, expr: &Expr) -> anyhow::Result<Value> {
        Ok(match expr {
            Expr::Comment(_) => Value::Void,
            Expr::CharLiteral(c) => Value::Char(*c),
            Expr::StringLiteral(s) => Value::List(s.chars().map(Value::Char).collect()),
            Expr::IntLiteral(i) => Value::Int(*i),
            Expr::If(cond) => self.eval_if(cond)?,
            Expr::While(cond) => self.eval_while(cond)?,
            Expr::ListCompEl(var_name) => self.eval_list_comp_el(var_name)?,
            Expr::ListCompIndex(var_name) => self.eval_list_comp_index(var_name)?,
            Expr::Length(expr) => Value::Int(self.eval_expr(expr)?.as_list()?.len() as _),
            Expr::Ref(var_name) => self.eval_ref(var_name)?,
            Expr::Block(expr) => self.eval_block(expr)?,
            Expr::Assignment(name, expr) => self.eval_assignment(name, expr)?,
            Expr::ListComprehension { list, over } => self.eval_list_comp(list, over)?,
            Expr::TupleLiteral(exprs) => Value::Tuple(
                exprs
                    .iter()
                    .map(|e| self.eval_expr(e))
                    .collect::<anyhow::Result<Vec<Value>>>()?,
            ),
            Expr::ListLiteral(exprs) => Value::List(
                exprs
                    .iter()
                    .map(|e| self.eval_expr(e))
                    .collect::<anyhow::Result<Vec<Value>>>()?,
            ),
            Expr::CallPat(get_pat, arg) => self.eval_call_pat(get_pat, arg)?,
            Expr::Range(low, high) => self.eval_range(low, high)?,
            Expr::BinOp(lhs, op, rhs) => self.eval_bin_op(lhs, *op, rhs)?,
        })
    }

    fn eval_call_pat(&mut self, get_pat: &Expr, arg: &Expr) -> anyhow::Result<Value> {
        self.push_block_scope();
        let pat = self.eval_expr(get_pat)?.as_pattern()?;
        self.pop_scope();
        Ok(todo!())
    }

    fn eval_bin_op(&mut self, lhs: &Expr, op: Op, rhs: &Expr) -> anyhow::Result<Value> {
        let lhs = self.eval_expr(lhs)?.as_int()?;
        let rhs = self.eval_expr(rhs)?.as_int()?;
        match op {
            Op::Add => Ok(Value::Int(lhs + rhs)),
            Op::Sub => Ok(Value::Int(lhs - rhs)),
            Op::Mul => Ok(Value::Int(lhs * rhs)),
            Op::Div => Ok(Value::Int(lhs / rhs)),
            Op::Eq => Ok(Value::Int((lhs == rhs).into())),
            Op::Neq => Ok(Value::Int((lhs != rhs).into())),
            Op::Lt => Ok(Value::Int((lhs < rhs).into())),
            Op::Lte => Ok(Value::Int((lhs <= rhs).into())),
            Op::Gt => Ok(Value::Int((lhs > rhs).into())),
            Op::Gte => Ok(Value::Int((lhs >= rhs).into())),
            Op::And => Ok(Value::Int((lhs != 0 && rhs != 0).into())),
            Op::Or => Ok(Value::Int((lhs != 0 || rhs != 0).into())),
            Op::Pow => Ok(Value::Int(lhs.pow(rhs as _))),
            Op::Shl => Ok(Value::Int(lhs << rhs)),
        }
    }

    fn eval_range(&mut self, low: &Expr, high: &Expr) -> anyhow::Result<Value> {
        let low = self.eval_expr(low)?.as_int()?;
        let high = self.eval_expr(high)?.as_int()?;
        Ok(Value::List((low..high).map(Value::Int).collect()))
    }

    fn eval_block(&mut self, block: &[Expr]) -> anyhow::Result<Value> {
        let mut res = Value::Void;
        self.push_block_scope();
        for expr in block {
            res = self.eval_expr(expr)?;
        }
        self.pop_scope();
        Ok(res)
    }

    fn eval_if(
        &mut self,
        Conditional { cond, then, r#else }: &Conditional,
    ) -> anyhow::Result<Value> {
        let cond_val = self.eval_expr(cond)?;
        if cond_val.is_true()? {
            let ret = self.eval_expr(then)?;
            if let Some(_) = r#else {
                Ok(ret)
            } else {
                Ok(Value::Void)
            }
        } else if let Some(r#else) = r#else {
            self.eval_expr(r#else)
        } else {
            Ok(Value::Void)
        }
    }

    fn eval_while(
        &mut self,
        Conditional { cond, then, .. }: &Conditional,
    ) -> anyhow::Result<Value> {
        let mut cond_val = self.eval_expr(cond)?;
        while cond_val.is_true()? {
            self.eval_expr(then)?;
            cond_val = self.eval_expr(cond)?;
        }
        Ok(Value::Void)
    }

    fn eval_ref(&mut self, var_name: &str) -> anyhow::Result<Value> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::Block(scope) => scope.get(var_name).cloned(),
                Scope::ListComp {
                    name: Some(name),
                    index: _,
                    list,
                } if name == var_name => Some(Value::List(list.clone())),
                Scope::ListComp { .. } => None,
            })
            .ok_or_else(|| anyhow::anyhow!("Variable {} not found", var_name))
    }

    fn eval_list_comp(&mut self, list: &Expr, over: &Expr) -> anyhow::Result<Value> {
        let name = match list {
            Expr::Ref(name) => Some(name.to_string()),
            _ => None,
        };
        let list = self.eval_expr(list)?.into_list()?;
        let len = list.len();
        let mut ret = Vec::with_capacity(len);
        self.push_list_comp_scope(name, list);
        for _ in 0..len {
            ret.push(self.eval_expr(over)?);
            self.incr_list_comp_index()?;
        }
        self.pop_scope();
        Ok(Value::List(ret))
    }

    fn eval_list_comp_el(&mut self, var_name: &str) -> anyhow::Result<Value> {
        self.scope
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::ListComp {
                    name: Some(name),
                    index,
                    list,
                } if name == var_name => list.get(*index).cloned(),
                _ => None,
            })
            .ok_or_else(|| anyhow::anyhow!("Variable {} not found", var_name))
    }

    fn eval_list_comp_index(&mut self, var_name: &str) -> anyhow::Result<Value> {
        // TODO: duped with eval_list_comp_el
        self.scope
            .iter()
            .rev()
            .find_map(|scope| match scope {
                Scope::ListComp {
                    name: Some(name),
                    index,
                    list: _,
                } if name == var_name => Some(Value::Int(*index as _)),
                _ => None,
            })
            .ok_or_else(|| anyhow::anyhow!("Variable {} not found", var_name))
    }

    fn eval_assignment(&mut self, var_name: &str, expr: &Expr) -> anyhow::Result<Value> {
        let val = self.eval_expr(expr)?;
        self.this_scope()?.set(var_name.to_owned(), val.clone())?;
        Ok(val)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Void,
    Char(char),
    Tuple(Vec<Value>),
    List(Vec<Value>),
    Int(i128),
    Pattern(Pattern),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Pattern(parser::PatDef);

struct MatchedPattern {
    bindings: Vec<(String, Value)>,
    expr: Expr,
}

impl Pattern {
    fn match_val(&self, val: Value) -> Option<MatchedPattern> {
        for match_arm in &self.0.matches {
            let binding = &match_arm.binding;
        }
        None
    }
}

#[derive(Debug, Clone)]
struct Match {
    value: Value,
    name: Option<String>,
    inner_matches: Vec<Match>,
}

impl Match {
    fn unnamed(value: Value) -> Self {
        Self {
            value,
            name: None,
            inner_matches: vec![],
        }
    }

    fn with_inner_matches(mut self, inner_matches: Vec<Match>) -> Self {
        self.inner_matches = inner_matches;
        self
    }

    fn add_name(mut self, name: String) -> Self {
        if self.has_name() {
            self.inner_matches.push(self.clone());
        }
        self.name = Some(name);
        self
    }

    fn has_name(&self) -> bool {
        self.name.is_some()
    }
}

fn match_binding(val: Value, binding: &parser::Binding) -> Option<Match> {
    match binding {
        Binding::Char(c) => {
            if matches!(Value::Char(*c), val) {
                Some(Match::unnamed(val))
            } else {
                None
            }
        }
        Binding::ListOf(_, _) | Binding::Concat(_, _) => {
            if let Value::List(vals) = &val {
                if let Some((matched, rest)) = match_list(vals.clone(), binding) {
                    if rest.is_empty() {
                        matched
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        Binding::Tuple(bindings) => {
            if let Value::Tuple(vals) = &val {
                let matches = vals
                    .iter()
                    .zip(bindings.iter())
                    .map(|(val, binding)| match_binding(val.clone(), binding))
                    .collect::<Vec<Option<Match>>>();
                if matches.iter().all(Option::is_some) {
                    let inner_matches = matches.into_iter().flatten().collect();
                    Some(Match::unnamed(val).with_inner_matches(inner_matches))
                } else {
                    None
                }
            } else {
                None
            }
        }
        Binding::Named(name, binding) => {
            match_binding(val, binding).map(|m| m.add_name(name.to_owned()))
        }
        Binding::Anything => Some(Match::unnamed(val)),
        Binding::Type(_) => unreachable!("types are unimplemented"),
        Binding::Ref(_) => unreachable!("refs are unimplemented"),
    }
}

// TODO: maybe this could work with iterators?
// returns the remaining unmatched list if any
fn match_list(vals: Vec<Value>, binding: &parser::Binding) -> Option<(Match, Vec<Value>)> {
    match binding {
        Binding::Anything | Binding::Ref(_) | Binding::Type(_) | Binding::Char(_) => {
            let val = vals.first()?;
            if let Some(matched) = match_binding(val.clone(), binding) {
                Some((matched, vals.into_iter().skip(1).collect()))
            } else {
                None
            }
        }
        Binding::Concat(left, right) => {
            let (mut left_matched, rest) = match_list(vals.clone(), left)?;
            let (right_matched, rest) = match_list(rest, right)?;
            let this_match = Match::unnamed(Value::List(vals))
                .with_inner_matches(vec![left_matched, right_matched]);
            Some((this_match, rest))
        }
        Binding::ListOf(binding, len_binding) => {
            let mut matched = vec![];
            let mut rest = vals;
            if let Some(ListLenBinding::Min(min)) = len_binding {
                for _ in 0..*min {
                    let (inner_match, inner_rest) = match_list(rest, binding)?;
                    matched.extend(inner_match);
                    rest = inner_rest;
                }
            }
            while let Some((inner_match, inner_rest)) = match_list(rest.clone(), binding) {
                matched.extend(inner_match);
                rest = inner_rest;
            }
            if !matched.is_empty() {
                Some((matched, rest))
            } else {
                None
            }
        }
        Binding::Tuple(bindings) => {
            let mut matched = vec![];
            let mut rest = vals;
            for binding in bindings {
                let (inner_match, inner_rest) = match_list(rest, binding)?;
                matched.extend(inner_match);
                rest = inner_rest;
            }
            Some((matched, rest))
        }
        Binding::Named(_, _) => panic!("what to do when getting a named in here???????"),
    }
}

impl Value {
    fn as_pattern(&self) -> anyhow::Result<&Pattern> {
        match self {
            Value::Pattern(p) => Ok(p),
            _ => Err(anyhow::anyhow!("not a pattern")),
        }
    }

    fn as_char(&self) -> anyhow::Result<char> {
        match self {
            Value::Char(c) => Ok(*c),
            _ => Err(anyhow::anyhow!("not a char")),
        }
    }

    fn as_int(&self) -> anyhow::Result<i128> {
        match self {
            Value::Int(i) => Ok(*i),
            _ => Err(anyhow::anyhow!("not an int")),
        }
    }

    fn as_list(&self) -> anyhow::Result<&Vec<Value>> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(anyhow::anyhow!("not a list")),
        }
    }

    fn into_list(self) -> anyhow::Result<Vec<Value>> {
        match self {
            Value::List(l) => Ok(l),
            _ => Err(anyhow::anyhow!("not a list")),
        }
    }

    fn is_true(&self) -> anyhow::Result<bool> {
        Ok(self.as_int()? != 0)
    }
}
