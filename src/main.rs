mod parser;

const TEST_PROGGY: &str = r#"
pat bin { '0' => 0, '1' => 1 }
pat line { bins@(['0']{1+}) ~ '\n' => bins }
pat enumerate { xs@([ANY]{num_lines}) => {
    ixs = abc
}}
"#;

fn main() -> anyhow::Result<()> {
    dbg!(parser::parse_program(TEST_PROGGY)?);
    Ok(())
}
