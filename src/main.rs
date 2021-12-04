mod parser;

const TEST_PROGGY: &str = r#"
'h'
'o'
1
2
69
if 1: 1
if 2: 1 else: 2
"#;

fn main() -> anyhow::Result<()> {
    dbg!(parser::parse_program(TEST_PROGGY)?);
    Ok(())
}
