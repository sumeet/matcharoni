use crate::interp::{Interpreter, Pattern, Value};
use dyn_partial_eq::DynPartialEq;
use itertools::Itertools;
use std::fmt::Write;
use std::fs::read_to_string;

pub fn builtins() -> Vec<(&'static str, Box<dyn Pattern>)> {
    vec![
        ("read_to_string", Box::new(ReadToString {})),
        ("dbg", Box::new(DebugPrint {})),
        ("push", Box::new(Push {})),
        ("exit", Box::new(Exit {})),
    ]
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct ReadToString {}

impl Pattern for ReadToString {
    fn name(&self) -> &str {
        "read_to_string"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Value) -> anyhow::Result<Value> {
        let filename = arg.as_string()?;
        Ok(Value::List(
            read_to_string(filename)?.chars().map(Value::Char).collect(),
        ))
    }
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct DebugPrint {}

impl Pattern for DebugPrint {
    fn name(&self) -> &str {
        "dbg"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Value) -> anyhow::Result<Value> {
        println!("{}", print(&arg));
        Ok(arg)
    }
}

pub fn print(arg: &Value) -> String {
    let mut s = String::new();
    match arg {
        Value::Void => write!(&mut s, "()"),
        Value::Char(c) => write!(&mut s, "'{}'", c),
        Value::Tuple(vals) => {
            write!(&mut s, "(").unwrap();
            if let Some((last, rest)) = vals.split_last() {
                for val in rest {
                    write!(&mut s, "{}", print(val)).unwrap();
                    write!(&mut s, ", ").unwrap();
                }
                write!(&mut s, "{}", print(last)).unwrap();
            }
            write!(&mut s, ")")
        }
        Value::List(vals) => {
            write!(&mut s, "[").unwrap();
            if let Some((last, rest)) = vals.split_last() {
                for val in rest {
                    write!(&mut s, "{}", print(val)).unwrap();
                    write!(&mut s, ", ").unwrap();
                }
                write!(&mut s, "{}", print(last)).unwrap();
            }
            write!(&mut s, "]")
        }
        Value::Int(n) => write!(&mut s, "{}", n),
        Value::Pattern(pat) => write!(&mut s, "<Pat {}>", pat.name()),
    }
    .unwrap();
    s
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct Push {}

impl Pattern for Push {
    fn name(&self) -> &str {
        "push"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Value) -> anyhow::Result<Value> {
        let tuple = arg.into_tuple()?;
        let (list, to_add) = tuple
            .into_iter()
            .collect_tuple()
            .ok_or_else(|| anyhow::anyhow!("wrong arguments"))?;
        let mut list = list.into_list()?;
        list.push(to_add);
        Ok(Value::List(list))
    }
}

#[derive(Debug, Clone, PartialEq, DynPartialEq)]
pub struct Exit {}

impl Pattern for Exit {
    fn name(&self) -> &str {
        "exit"
    }

    fn match_full(&self, _: &mut Interpreter, arg: Value) -> anyhow::Result<Value> {
        std::process::exit(arg.as_int()? as _);
    }
}
