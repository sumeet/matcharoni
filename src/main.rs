mod parser;

const TEST_PROGGY: &str = r#"
// TODO: error if wrong number of bindings in list
// TODO: not going to use bind for now
// bind { xs@([$names]) } => [*$names = xs[#$names] <- $names]

pat bin { '0' => 0, '1' => 1 }
// TODO: might need flatten thing...
pat line { [bins@bin]{1+} ~ '\n' => bins }
// TODO: might need flatten thing...
pat lines { [lines@line] => lines }

pat bin_le_to_int { [bits@Int] => {
    sum([shl(sub(#bits, %bits), *bits) <- bits])
}}


input = read_to_string("./input")
readings = lines(input)

pat expand { (x@ANY, n@Int) => { [x <- 0..n] } }

//pat calc_most_commons { [sums@Int] => 
//  [(*sums / 2) > (#readings / 2) <- sums]
//}}
//
//pat calc_least_commons { [sums@Int] => 
//  [(*sums / 2) < (#readings / 2) <- sums]
//}}
//
//sums = expand(0, len(readings(0)))
"#;

fn main() -> anyhow::Result<()> {
    dbg!(parser::parse_program(TEST_PROGGY)?);
    Ok(())
}
