use crate::baserules::board_rep::{BaseMove, PossibleMove};
use crate::baserules::castling::Castling::{
    BlackKingSide, BlackQueenSide, WhiteKingSide, WhiteQueenSide,
};
use crate::baserules::piece_color::PieceColor;
use crate::baserules::piece_kind::PieceKind::{King, Queen};
use crate::baserules::piece_state::PieceState;
use crate::util::{AnyError, IntResult, TryWithPanic};
use enum_map::{enum_map, Enum, EnumMap};
use enumset::{enum_set, EnumSet, EnumSetType};
use lazy_static::lazy_static;

#[derive(EnumSetType, Debug, Enum)]
pub enum Castling {
    WhiteKingSide,
    WhiteQueenSide,
    BlackKingSide,
    BlackQueenSide,
}

impl Castling {
    pub fn fen_char(&self) -> char {
        match self {
            WhiteKingSide => 'K',
            WhiteQueenSide => 'Q',
            BlackKingSide => 'k',
            BlackQueenSide => 'q',
        }
    }
    pub fn from_notation(notation: &str, color: PieceColor) -> IntResult<Castling> {
        let castle_type = notation.split('-').count();
        let piece_rep = match castle_type {
            2 => PieceState { kind: King, color },
            3 => PieceState { kind: Queen, color },
            _ => return Err(format!("Incorrect notation for castling: {notation}").into()),
        };
        format!("{piece_rep}").chars().next().unwrap().try_into()
    }
    pub fn from_king_move(&self, king_move: BaseMove) -> Option<PossibleMove> {
        let castling_move: &PossibleMove = (*self).into();
        if king_move == castling_move.the_move {
            Some(*castling_move)
        } else {
            None
        }
    }
}

lazy_static! {
    static ref CASTLING_MOVE_MAP: EnumMap<Castling, PossibleMove> = enum_map! {
        WhiteKingSide => PossibleMove {
            the_move: "e1g1".transform(),
            pawn_promotion: None,
            rook: Some("h1f1".transform()),
        },
        WhiteQueenSide => PossibleMove {
            the_move: "e1c1".transform(),
            pawn_promotion: None,
            rook: Some("a1d1".transform()),
        },
        BlackKingSide => PossibleMove {
            the_move: "e8g8".transform(),
            pawn_promotion: None,
            rook: Some("h8f8".transform()),
        },
        BlackQueenSide => PossibleMove {
            the_move: "e8c8".transform(),
            pawn_promotion: None,
            rook: Some("a8d8".transform()),
        },
    };
}

impl From<Castling> for &PossibleMove {
    fn from(value: Castling) -> Self {
        &CASTLING_MOVE_MAP[value]
    }
}

impl TryFrom<char> for Castling {
    type Error = AnyError;

    fn try_from(value: char) -> IntResult<Self> {
        match value {
            'K' => Ok(WhiteKingSide),
            'Q' => Ok(WhiteQueenSide),
            'k' => Ok(BlackKingSide),
            'q' => Ok(BlackQueenSide),
            _ => Err(format!("Invalid castling type: {value}").into()),
        }
    }
}

pub const fn white_can_castle() -> EnumSet<Castling> {
    enum_set!(WhiteKingSide | WhiteQueenSide)
}

pub const fn black_can_castle() -> EnumSet<Castling> {
    enum_set!(BlackKingSide | BlackQueenSide)
}

pub const fn queenside_castle() -> EnumSet<Castling> {
    enum_set!(BlackQueenSide | WhiteQueenSide)
}

pub const fn kingside_castle() -> EnumSet<Castling> {
    enum_set!(WhiteKingSide | BlackKingSide)
}
