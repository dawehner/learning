struct Board {
    tokens: Vec<Token>,
}

struct BoardPos {
    x: uint,
    y: uint,
}

struct Token {
    pos: BoardPos,
    player: uint,
}

struct Player {
    name: String
}

fn board_create_empty() -> Board {
    let mut tokens: Vec<Token> = vec![];
    // Iterate over players.
    for p in range(0u, 1u) {
        // Iterate over rows.
        for y in range(0u, 3u) {
            // Iterate over columns.
            for x in range(0u, 4u) {
                let pos = BoardPos {x: calc_init_pos_x(p, y, x), y: calc_init_pos_y(p, y, x)  };
                let token = Token { pos: pos, player: p };
            }
        }
    }
    let board = Board { tokens: tokens };
    return board;
}

fn calc_init_pos_x(player: uint, x: uint, y: uint) -> uint {
    let player_offset = player;
    match y {
        0 => x * 2 + player_offset,
        1 => x * 2 + 1 + player_offset,
        2 => x * 2 + player_offset,
        _ => x * 2 + player_offset,
    }
}
fn calc_init_pos_y(player: uint, x: uint, y: uint) -> uint {
    if player == 0u {
        y
    }
    else {
        7 - y
    }
}

#[test]
fn test_calc_init_pos() {
    assert!(calc_init_pos_x(0, 0, 0) == 0);
    assert!(calc_init_pos_y(0, 0, 0) == 0);

    assert!(calc_init_pos_x(1, 0, 0) == 1);
    assert!(calc_init_pos_y(1, 0, 0) == 7);

    assert!(calc_init_pos_x(0, 1, 0) == 2);
    assert!(calc_init_pos_y(0, 1, 0) == 0);

    assert!(calc_init_pos_x(0, 0, 1) == 1);
    assert!(calc_init_pos_y(0, 0, 1) == 1);

    assert!(calc_init_pos_x(0, 2, 1) == 5);
    assert!(calc_init_pos_y(0, 2, 1) == 1);

    assert!(calc_init_pos_x(1, 2, 1) == 4);
    assert!(calc_init_pos_y(1, 2, 1) == 6);
}

fn main() {
    println!("Hello, world!");
}
