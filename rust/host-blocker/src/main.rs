use quicli::prelude::*;
use std::result::Result;
use structopt::StructOpt;
mod hostfile;
use hostfile::*;

#[derive(Debug, StructOpt)]
#[structopt(name = "command", about = "The used command")]
enum Command {
    #[structopt(name = "list")]
    CommandList,
}

fn read_hosts() -> Result<String, std::io::Error> {
    let content = std::fs::read_to_string("/tmp/test");
    return content;
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
            println!("{:?}", output);
            Ok(())
        }
        Err(err) => panic!("Error parsing hostfile, good luck {:?}", err),
    }
}
