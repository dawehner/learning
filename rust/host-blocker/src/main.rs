use quicli::prelude::*;
use std::result::Result;
use structopt::StructOpt;
mod hostfile;
use hostfile::*;
mod simplify;

#[derive(Debug, StructOpt)]
#[structopt(name = "command", about = "The used command")]
enum Command {
    #[structopt(name = "list")]
    CommandList,
}

fn read_hosts() -> Result<String, std::io::Error> {
    std::fs::read_to_string("/etc/hosts")
}

/// Read some lines of a file
#[derive(Debug, StructOpt)]
struct Cli {}

fn main() -> CliResult {
    let hosts = read_hosts();
    let result = match &hosts {
        Ok(string) => parse_hosts(&string),
        Err(error) => panic!("Problem reading the file {:?}", error),
    };

    match result {
        Ok((_input, output)) => {
            println!("{}", simplify::simplify(output));
            Ok(())
        }
        Err(err) => panic!("Error parsing hostfile, good luck {:?}", err),
    }
}
