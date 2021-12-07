use crate::interp::{Interpreter, Pattern, Value};
use dyn_partial_eq::DynPartialEq;
use itertools::Itertools;
use std::fs::read_to_string;

pub fn builtins() -> Vec<(&'static str, Box<dyn Pattern>)> {
    vec![
        ("read_to_string", Box::new(ReadToString {})),
        ("dbg", Box::new(DebugPrint {})),
        ("push", Box::new(Push {})),
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
        println!("{:?}", arg);
        Ok(arg)
    }
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
