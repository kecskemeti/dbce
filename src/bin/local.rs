use dbce::baserules::board::MATE;
use dbce::engine::{Engine, GameState};
use dbce::human_facing::helper::calculate_move_for_console;
use rand::{thread_rng, Rng};
use std::time::Duration;

fn make_machine_move(engine: &mut Engine, gamestate: &mut GameState) {
    println!("It's my move now, let me think:");
    let to_move = calculate_move_for_console(engine, gamestate, &Duration::from_secs(10))
        .1
         .0
        .unwrap();
    gamestate.make_a_generated_move(&to_move);
}

fn main() {
    let input = std::io::stdin();
    println!("Custom starting position?");
    let mut line = String::new();
    {
        input.read_line(&mut line).unwrap();
    }
    let (mut engine, mut gamestate) = if line.trim().to_lowercase().starts_with('y') {
        println!("What is the FEN of the starting position?");
        let mut line = String::new();
        input.read_line(&mut line).unwrap();
        Engine::from_fen(&line)
    } else {
        Engine::new()
    };
    let machine_moves_first: bool = thread_rng().gen();
    if machine_moves_first {
        make_machine_move(&mut engine, &mut gamestate);
    }
    while (gamestate.get_board().score.abs() - MATE).abs() > 1.0 {
        println!("Current board: {}", gamestate.get_board());
        loop {
            println!("What's your move?");
            let mut line = String::new();
            {
                input.read_line(&mut line).unwrap();
                if gamestate.make_a_human_move(line.trim()).is_ok() {
                    break;
                }
            }
            println!("Sorry, I can't understand {line}");
        }
        println!("Current board: {}", gamestate.get_board());
        make_machine_move(&mut engine, &mut gamestate);
    }
}
