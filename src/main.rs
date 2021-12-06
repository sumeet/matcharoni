#![feature(destructuring_assignment)]

mod interp;
mod parser;

const TEST_PROGGY: &str = r#"
// TODO: error if wrong number of bindings in list
// TODO: not going to use bind for now
// bind { xs@([$names]) } => [*$names = xs[#$names] <- $names]

pat bin { '0' => 0, '1' => 1 }
pat reading { bins@[bin]{1+} ~ '\n' => bins }
pat readings { lines@[reading] => lines }

input = read_to_string("./input")
readings = readings(input)

pat expand { (x@ANY, n@ANY) => { [x <- 0..n] } }
pat calc_most_commons { [sums@ANY] =>  {
  [(*sums / 2) < (#readings / 2) <- sums]
}}
pat calc_least_commons { [sums@ANY] =>  {
  [(*sums / 2) < (#readings / 2) <- sums]
}}
pat bin_le_to_int { [bits@ANY] => {
  sum [(*bits << (#bits - %bits)) <- bits]
}}

sums = expand(0, #(readings(0)))
for reading <- readings {
  for bit <- bits {
    sums[#bit] = sums[#bit] + bit
  }
}
"#;

fn main() -> anyhow::Result<()> {
    let program = parser::parse_program(TEST_PROGGY)?;
    let mut interp = interp::Interpreter::new();
    for statement in &program.statements {
        interp.eval_statement(statement)?;
    }
    Ok(())
}
