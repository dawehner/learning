use std::old_io::File;
use std::vec;
use std::string::String;

fn main() {
    println!("Hello, world!");

    let path = Path::new("test.cvs");
    let mut file = match File::open(&path) {
        Err(why) => panic!("could not open file: {}", why),
        Ok(file) => file,
    };

    let mut rows = vec![];
 
    match file.read_to_string() {
        Err(why) => panic!("arg"),
        Ok(string) => {
            let mut row = vec![];
            for value in  string.split(",") {
                row.push(String::from_str(value));
            }
            rows.push(row)
        }
    }

    for row in rows {
        for string in row {

            println!("{}", string);
        }
    }

}
