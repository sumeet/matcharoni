mod parser;

const TEST_PROGGY: &str = r#"
pat enumerate { xs@([ANY]{num_lines}) => {
    ixs = []
    i = 0
    while lt(i, num_lines): {
        push(ixs, i)
        i = add(i, 1)
    }
    ixs
}}

pat bin { '0' => 0, '1' => 1 }
pat line { bins@([bin]{1+}) ~ '\n' => bins }
pat lines { lines@([line]{num_lines}) => [enumerate(lines), num_lines] }

pat bin_le_to_int { bits@([ANY]{len}) => {
    sum = 0
    factor = 1
    while gte(len, 0): {
      len  = minus(len, 1)
      sum = add(sum, mul(factor, bits(len)))
      factor = mul(factor, 8)
    }
    sum
}}

input = read_to_string("./input")

"#;

fn main() -> anyhow::Result<()> {
    dbg!(parser::parse_program(TEST_PROGGY)?);
    Ok(())
}
