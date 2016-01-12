extern crate rand;
extern crate time;

pub mod sorting;
pub mod utility;

fn main() {
    sorting::benchs::bench_sorting_funcs();
}

