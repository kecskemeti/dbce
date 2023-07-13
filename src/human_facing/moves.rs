/*
 *  ========================================================================
 *  DBCE chess bot, translator of moves received from external sources
 *  ========================================================================
 *
 *  This file is part of DBCE.
 *
 *  DBCE is free software: you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License as published
 *  by the Free Software Foundation, either version 3 of the License, or (at
 *  your option) any later version.
 *
 *  DBCE is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License along
 *  with DBCE.  If not, see <http://www.gnu.org/licenses/>.
 *
 *  (C) Copyright 2022-3, Gabor Kecskemeti
 */
use crate::baserules::board_rep::{BaseMove, PossibleMove};
use crate::baserules::castling::Castling;
use crate::baserules::piece_kind::PieceKind;
use crate::baserules::piece_kind::PieceKind::King;
use crate::baserules::positions::AbsoluteBoardPos;
use crate::engine::continuation::BoardContinuation;
use crate::util::{IntResult, TryWithPanic};

fn accept_promotion(promote_kind: &Option<PieceKind>, candidate_move: &PossibleMove) -> bool {
    promote_kind.map_or_else(
        || candidate_move.pawn_promotion.is_none(),
        |knd| {
            candidate_move
                .pawn_promotion
                .map_or(false, |ppm| knd == ppm)
        },
    )
}

/// Reads short algebraic notation and translates it to our internal structures
/// <https://en.wikipedia.org/wiki/Algebraic_notation_(chess)>
pub fn make_a_human_move(board: BoardContinuation, the_move: &str) -> Option<BoardContinuation> {
    let mut rev_move: String = the_move.chars().rev().collect();
    let first_char = rev_move.pop().unwrap();
    let mut col = Vec::new();
    let mut piece_kind = PieceKind::Pawn;
    let mut castling = false;
    match first_char {
        'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => {
            col.push(AbsoluteBoardPos::parse_column(first_char.try_into().unwrap()).unwrap())
        }
        'K' | 'Q' | 'N' | 'R' | 'B' => piece_kind = PieceKind::from_char(first_char),
        'O' => castling = true,
        _ => panic!("Unexpected chess notation"),
    }
    let found_move = if castling {
        let castling_done = Castling::from_notation(the_move, board.who_moves).unwrap();
        let castling_move: &'static PossibleMove = castling_done.into();
        Some(*castling_move)
    } else {
        let mut row = Vec::new();
        let mut promotion = false;
        let mut promote_kind = None;
        loop {
            let first_char = rev_move.pop();
            if let Some(consecutive) = first_char {
                match consecutive {
                    'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => col.push(
                        AbsoluteBoardPos::parse_column(consecutive.try_into().unwrap()).unwrap(),
                    ),
                    'x' | '+' | '#' => {
                        // Takes, check, and mate are not relevant here, we already handle them with move-gen below
                    }
                    '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => row.push(
                        AbsoluteBoardPos::parse_row(consecutive.try_into().unwrap()).unwrap(),
                    ),
                    '=' => promotion = true,
                    'Q' | 'R' | 'B' | 'K' => {
                        if promotion {
                            promote_kind = Some(PieceKind::from_char(consecutive));
                        }
                    }
                    _ => panic!("Unexpected notation in second char"),
                }
            } else {
                break;
            }
        }
        let mut all_moves = Vec::new();
        board.gen_potential_moves(&mut all_moves);
        let target = (row.pop().unwrap(), col.pop().unwrap()).transform();
        let filter: Box<dyn Fn(&Option<PieceKind>, &PossibleMove) -> bool> = if row.is_empty() {
            if col.is_empty() {
                // regular move, no clarification needed for the moving piece
                Box::new(accept_promotion)
            } else {
                // Clarification enough to contain extra column detail
                let source_col = col.pop().unwrap();
                Box::new(move |promote_kind, candidate_move| {
                    candidate_move.the_move.from.1 == source_col
                        && accept_promotion(promote_kind, candidate_move)
                })
            }
        } else if col.is_empty() {
            // Clarification enough to contain extra row detail, can't apply to pawns, no promotion handling needed
            let source_row = row.pop().unwrap();
            Box::new(move |_, candidate_move| candidate_move.the_move.from.0 == source_row)
        } else {
            // Move needs all coordinates as it would be ambiguous otherwise, can't apply to pawns, no promotion handling needed
            let source = (row.pop().unwrap(), col.pop().unwrap()).transform();
            Box::new(move |_, candidate_move| candidate_move.the_move.from == source)
        };

        all_moves.into_iter().find(|candidate_move| {
            let moving_piece = board[candidate_move.the_move.from].as_ref().unwrap();
            moving_piece.kind == piece_kind
                && candidate_move.the_move.to == target
                && filter(&promote_kind, candidate_move)
        })
    };
    found_move.map(|im| board.make_cached_move(&im))
}

/// Allows moves to be translated from lichess to our internal representation
/// See also: <https://en.wikipedia.org/wiki/Universal_Chess_Interface>
/// # Panics
/// when the received uci move is out of the usual uci move format's length constraints
///
/// # Example
/// ```
/// use dbce::baserules::board::PSBoard;
/// use dbce::engine::continuation::BoardContinuation;
/// use dbce::human_facing::moves::make_an_uci_move;
/// let opera_game = BoardContinuation::new(PSBoard::from_fen("1n2kb1r/p4ppp/4q3/4p1B1/4P3/8/PPP2PPP/2KR4 w k - 0 17"));
/// let opera_result = make_an_uci_move(opera_game, "d1d8").unwrap();
/// assert_eq!("1n1Rkb1r/p4ppp/4q3/4p1B1/4P3/8/PPP2PPP/2K5 b k - 1 17", opera_result.to_fen());
/// ```
pub fn make_an_uci_move(board: BoardContinuation, the_move: &str) -> IntResult<BoardContinuation> {
    let len = the_move.len();
    assert!(len < 6 && len > 3);
    let raw_move = the_move.as_bytes();
    let the_move = BaseMove::from_uci(the_move)?;
    let the_complete_move = if len == 5 {
        let promote_kind = PieceKind::from_char(raw_move[4] as char);
        PossibleMove {
            the_move,
            pawn_promotion: Some(promote_kind),
            rook: None,
        }
    } else {
        let moving_piece = board[the_move.from].as_ref().unwrap();
        let simple_move = PossibleMove::simple_move(the_move);
        if moving_piece.kind == King {
            board
                .castling
                .iter()
                .find_map(|c| c.from_king_move(the_move))
                .unwrap_or(simple_move)
        } else {
            simple_move
        }
    };
    Ok(board.make_cached_move(&the_complete_move))
}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::engine::continuation::BoardContinuation;
    use crate::human_facing::moves::{make_a_human_move, make_an_uci_move};

    #[test]
    fn test_rook_takes() {
        let board = PSBoard::from_fen("1Rb1r1k1/p4ppp/3p4/P7/2RPPP2/2K5/7r/8 w - - 0 30");
        let cont = BoardContinuation::new(board);
        let result = make_a_human_move(cont, "Rcxc8");
        assert!(result.is_some());
        assert_eq!(
            "1RR1r1k1/p4ppp/3p4/P7/3PPP2/2K5/7r/8 b - - 0 30",
            result.unwrap().to_fen()
        );
    }

    fn test_castle_base(premove: Option<&str>, uci: &str, exp_black: &str, exp_white: &str) {
        let board =
            PSBoard::from_fen("r3k2r/pbpqppbp/1pnp1np1/8/8/1PNP1NP1/PBPQPPBP/R3K2R w KQkq - 2 9");
        let mut cont = BoardContinuation::new(board);
        if let Some(white_move) = premove {
            let after_move = make_an_uci_move(cont, white_move);
            assert!(after_move.is_ok());
            cont = after_move.unwrap();
        }
        let result = make_an_uci_move(cont, uci);
        assert!(result.is_ok());
        assert_eq!(
            format!("{exp_black}/pbpqppbp/1pnp1np1/8/8/1PNP1NP1/PBPQPPBP/{exp_white}"),
            result.unwrap().raw.to_fen_prefix()
        );
    }

    #[test]
    fn test_black_castle_queenside() {
        test_castle_base(Some("e1g1"), "e8c8", "2kr3r", "R4RK1");
    }

    #[test]
    fn test_black_castle_kingside() {
        test_castle_base(Some("e1g1"), "e8g8", "r4rk1", "R4RK1");
    }
    #[test]
    fn test_white_castle_queenside() {
        test_castle_base(None, "e1c1", "r3k2r", "2KR3R");
    }
    #[test]
    fn test_white_castle_kingside() {
        test_castle_base(None, "e1g1", "r3k2r", "R4RK1");
    }
}
