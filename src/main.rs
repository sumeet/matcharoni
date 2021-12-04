mod parser;

const TEST_PROGGY: &str = r#"
'h'
'o'
1
2
69
if 1: 1
if 2: 1 else: 2

pat bin {
  '0' => 0,
  '1' => 1,
  '2' ~ '3' => 2,
  c@('4') => c,
  xs@(['4']) => xs,
  ['4']{2+} => 123,
  ['4']{num_fours} => num_fours
}
"#;

fn main() -> anyhow::Result<()> {
    dbg!(parser::parse_program(TEST_PROGGY)?);
    Ok(())
}
