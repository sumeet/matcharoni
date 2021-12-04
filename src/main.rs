mod parser;

const TEST_PROGGY: &str = r#"
pat bin { '0' => 0, '1' => 1 }
pat line { bins@(['0']{1+}) ~ '\n' => bins }
pat enumerate { xs@([ANY]{num_lines}) => {
    ixs = []
    i = 0
    while lt(i, num_lines): {
        push(ixs, i)
        i = add(i, 1)
    }
    ixs
}}
"#;

fn main() -> anyhow::Result<()> {
    dbg!(parser::parse_program(TEST_PROGGY)?);
    Ok(())
}
