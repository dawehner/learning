use std::io;
use console::Term;
use dialoguer::{Input, Select};
use reqwest::header::{HeaderMap};

fn execute() -> io::Result<()> {
    let term = Term::stdout();
    term.write_line("Hello World!")?;

    let source = Input::<String>::new().with_prompt("URL to publish").interact()?;
    let target = Input::<String>::new().with_prompt("URL to publish to").interact()?;

    let items = [
        "https://brid.gy/publish/mastodon",
        "https://brid.gy/publish/twitter"
    ];
    let endpoint = Select::new()
        .with_prompt("Choose endpoint")
        .items(&items)
        .default(0)
        .interact()?;

    println!("source: {}", source);
    println!("target: {}", target);

    send_webmention(source, target, items[endpoint].to_string())
}

fn send_webmention(source: String, target: String, endpoint : String) -> io::Result<()> {
    let mut headers = HeaderMap::new();

    headers.insert("source", source.parse().unwrap());
    headers.insert("target", target.parse().unwrap());

    let client = reqwest::blocking::Client::new();
    let result = client.get(&endpoint)
        .headers(headers)
        .send();
    if let Ok(res) = result {
            println!("{:#?}", res);
    }
    else {
        println!("Error")
    }

    Ok(())
}

fn main() {
   execute().unwrap()
}
