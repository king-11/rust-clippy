// run-rustfix

#![warn(clippy::map_then_unwrap)]
#![allow(clippy::map_identity, clippy::let_unit_value)]

fn main() {
    // test code goes here
    let option: Option<i32> = None;
    let _ = option.map(|x| 1 + x * 2).unwrap(); // should trigger lint
}
