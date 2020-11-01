use serde::{Deserialize, Serialize};
use std::net::TcpListener;
use std::thread;

use tungstenite::accept_hdr;
use tungstenite::handshake::server::{Request, Response};

use websocket::sync::Server;
use websocket::OwnedMessage;

// Example json
/**
{
    color: "black",
    from: "A2",
    to: "A3"
}
 **/

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
enum PieceColor {
    White = 0,
    Black = 1,
}

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
enum Column {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    E = 4,
    F = 5,
    G = 6,
    H = 7,
}

use i8 as Row;

#[derive(Serialize, Deserialize, Debug, Copy, Clone)]
struct Move {
    color: PieceColor,
    from: (Column, Row),
    to: (Column, Row),
}

fn main() {
    // let exampleMove : Move = Move { color : PieceColor::White, from : (Column::A, 2), to : (Column::A, 3) };

    let server = TcpListener::bind("127.0.0.1:2794").unwrap();

    for stream in server.incoming() {
        thread::spawn(move || {
            let callback = |req : &Request, mut response : Response| {
                println!("Recieved a new ws handshake");
                println!("The request's path is: {}", req.uri().path());
                println!("The request's headers are:");
                for (ref header, _value) in req.headers() {
                    println!("* {}", header);
                }

                
            }
        });
    }

    for request in server.filter_map(Result::ok) {
        // New thread for each connction for now.
        thread::spawn(move || {
            println!("{:?}", request.protocols());
            if !request.protocols().contains(&"chess-websocket".to_string()) {
                request.reject().unwrap();
                return;
            }

            // Accepts the connection itself.
            let mut client = request.use_protocol("chess-websocket").accept().unwrap();
            let ip = client.peer_addr().unwrap();

            println!("Connection from {}", ip);

            let message = OwnedMessage::Text("Hello".to_string());
            client.send_message(&message).unwrap();

            let (mut receiver, mut sender) = client.split().unwrap();

            for message in receiver.incoming_messages() {
                if message.is_err() {
                    println!("Message problem: {:?}", message);
                }
                let message = message.unwrap();

                match message {
                    OwnedMessage::Close(_) => {
                        let message = OwnedMessage::Close(None);
                        sender.send_message(&message).unwrap();
                        println!("Client {} disconnected", ip);
                        return;
                    }

                    OwnedMessage::Ping(ping) => {
                        let message = OwnedMessage::Pong(ping);
                        sender.send_message(&message).unwrap();
                    }

                    OwnedMessage::Text(string) => {
                        let ws_move: Move = serde_json::from_str(&string).unwrap();

                        println!("{:?}", ws_move);

                        let s = serde_json::to_string(&ws_move).unwrap();
                        let message = OwnedMessage::Text(s);

                        sender.send_message(&message).unwrap();
                    }

                    _ => {
                        sender.send_message(&message).unwrap();
                    }
                }
            }
        });
    }
}
