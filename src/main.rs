mod interp;
mod parser;

fn main() -> anyhow::Result<()> {
    let program_str = std::fs::read_to_string("./scratch.roni")?;
    let program = parser::parse_program(&program_str)?;
    let mut interp = interp::Interpreter::new();
    for statement in &program.statements {
        interp.eval_statement(statement)?;
    }
    Ok(())
}
