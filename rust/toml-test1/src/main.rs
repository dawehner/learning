extern crate toml;

use toml::Parser;
use std::old_io as io;
use std::old_io::File;


fn main() {
    let file_contents = String::from_utf8(File::open(&Path::new("Cargo.toml")).read_to_end().unwrap()).unwrap();

    let value = Parser::new(file_contents.as_slice()).parse().unwrap();
    println!("{:?}", value);
}
