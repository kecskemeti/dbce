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
use crate::baserules::piece_color::PieceColor::White;
use crate::baserules::piece_kind::PieceKind;
use crate::baserules::piece_kind::PieceKind::King;
use crate::engine::continuation::BoardContinuation;
use crate::util::IntResult;

/// Reads short algebraic notation and translates it to our internal structures
/// <https://en.wikipedia.org/wiki/Algebraic_notation_(chess)>
pub fn make_a_human_move(board: BoardContinuation, the_move: &str) -> Option<BoardContinuation> {
    let mut rev_move: String = the_move.chars().rev().collect();
    let first_char = rev_move.pop().unwrap();
    let mut col = Vec::new();
    let mut piece_kind = PieceKind::Pawn;
    let mut castling = false;
    let mut internal_move = None;
    match first_char {
        'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => col.push(first_char as u8 - b'a'),
        'K' | 'Q' | 'N' | 'R' | 'B' => piece_kind = PieceKind::from_char(first_char),
        'O' => castling = true,
        _ => panic!("Unexpected chess notation"),
    }
    if castling {
        let castle_type = the_move.split('-').count();
        let castle_row = if board.who_moves == White { 0 } else { 7 };
        if castle_type == 2 {
            //short
            internal_move = Some(PossibleMove {
                the_move: BaseMove {
                    from: (castle_row, 4).try_into().unwrap(),
                    to: (castle_row, 6).try_into().unwrap(),
                },
                pawn_promotion: None,
                rook: Some(BaseMove {
                    from: (castle_row, 7).try_into().unwrap(),
                    to: (castle_row, 5).try_into().unwrap(),
                }),
            });
        } else {
            //long
            internal_move = Some(PossibleMove {
                the_move: BaseMove {
                    from: (castle_row, 4).try_into().unwrap(),
                    to: (castle_row, 2).try_into().unwrap(),
                },
                pawn_promotion: None,
                rook: Some(BaseMove {
                    from: (castle_row, 0).try_into().unwrap(),
                    to: (castle_row, 3).try_into().unwrap(),
                }),
            });
        }
    } else {
        let mut row = Vec::new();
        let mut promotion = false;
        let mut promote_kind = None;
        loop {
            let first_char = rev_move.pop();
            if let Some(consecutive) = first_char {
                match consecutive {
                    'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' => {
                        col.push(consecutive as u8 - b'a')
                    }
                    'x' | '+' | '#' => {}
                    '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' => {
                        row.push(consecutive as u8 - b'1')
                    }
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
        if row.len() == 1 {
            if col.len() == 1 {
                let target = (row.pop().unwrap(), col.pop().unwrap());
                // regular move
                for a_move in all_moves {
                    let moving_piece = board.get_loc(a_move.the_move.from).as_ref().unwrap();
                    if moving_piece.kind == piece_kind
                        && a_move.the_move.to.0 == target.0
                        && a_move.the_move.to.1 == target.1
                    {
                        if promotion {
                            if let Some(knd) = promote_kind {
                                if let Some(ppm) = &a_move.pawn_promotion {
                                    if knd == *ppm {
                                        internal_move = Some(a_move);
                                        break;
                                    }
                                }
                            } else {
                                internal_move = Some(a_move);
                                break;
                            }
                        } else {
                            internal_move = Some(a_move);
                            break;
                        }
                    }
                }
            } else {
                let target = (row.pop().unwrap(), col.pop().unwrap());
                let source_col = col.pop().unwrap();
                for a_move in all_moves {
                    let moving_piece = board.get_loc(a_move.the_move.from).as_ref().unwrap();
                    if moving_piece.kind == piece_kind
                        && a_move.the_move.from.1 == source_col
                        && a_move.the_move.to.0 == target.0
                        && a_move.the_move.to.1 == target.1
                    {
                        if promotion {
                            if let Some(knd) = promote_kind {
                                if let Some(ppm) = &a_move.pawn_promotion {
                                    if knd == *ppm {
                                        internal_move = Some(a_move);
                                        break;
                                    }
                                }
                            } else {
                                internal_move = Some(a_move);
                                break;
                            }
                        } else {
                            internal_move = Some(a_move);
                            break;
                        }
                    }
                }
            }
        } else if col.len() == 1 {
            let target = (row.pop().unwrap(), col.pop().unwrap());
            let source_row = row.pop().unwrap();
            for a_move in all_moves {
                let moving_piece = board.get_loc(a_move.the_move.from).as_ref().unwrap();
                if moving_piece.kind == piece_kind
                    && a_move.the_move.from.0 == source_row
                    && a_move.the_move.to.0 == target.0
                    && a_move.the_move.to.1 == target.1
                {
                    internal_move = Some(a_move);
                    break;
                }
            }
        } else {
            let target = (row.pop().unwrap(), col.pop().unwrap());
            let source = (row.pop().unwrap(), col.pop().unwrap());
            for a_move in all_moves {
                let moving_piece = board.get_loc(a_move.the_move.from).as_ref().unwrap();
                if moving_piece.kind == piece_kind
                    && a_move.the_move.from.0 == source.0
                    && a_move.the_move.from.1 == source.1
                    && a_move.the_move.to.0 == target.0
                    && a_move.the_move.to.1 == target.1
                {
                    internal_move = Some(a_move);
                    break;
                }
            }
        }
    }
    internal_move.map(|im| board.make_cached_move(&im))
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
        let moving_piece = board.get_loc(the_move.from).as_ref().unwrap();
        let rook_from = (the_move.from.0, if the_move.to.1 == 6 { 7 } else { 0 })
            .try_into()
            .unwrap();
        let rook_to = (the_move.from.0, if the_move.to.1 == 6 { 5 } else { 3 })
            .try_into()
            .unwrap();
        PossibleMove {
            the_move,
            pawn_promotion: None,
            rook: if !board.castling.is_empty()
                && (the_move.to.0 == 0 || the_move.to.0 == 7)
                && the_move.from.1 == 4
                && (the_move.to.1 == 6 || the_move.to.1 == 2)
                && moving_piece.kind == King
            {
                Some(BaseMove {
                    from: rook_from,
                    to: rook_to,
                })
            } else {
                None
            },
        }
    };
    Ok(board.make_cached_move(&the_complete_move))
}

#[cfg(test)]
mod test {
    use crate::baserules::board::PSBoard;
    use crate::engine::continuation::BoardContinuation;
    use crate::human_facing::moves::make_a_human_move;

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
}
