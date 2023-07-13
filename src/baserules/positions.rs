use crate::util::{AnyError, IntResult, TryWithPanic};
use std::fmt::{Display, Formatter};
use std::ops;

/// Allows representing relative locations on the board (hence the signedness)
#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug, Default)]
pub struct RelativeBoardPos(pub i8, pub i8);

impl TryFrom<(i8, i8)> for RelativeBoardPos {
    type Error = AnyError;
    /// Allows loading absolute board coordinates to a BoardPos struct
    fn try_from((row, col): (i8, i8)) -> Result<Self, Self::Error> {
        if (-8..8).contains(&row) && (-8..8).contains(&col) {
            Ok(RelativeBoardPos(row, col))
        } else {
            Err(format!("Row and column index out of range for {row},{col}").into())
        }
    }
}

/// Used to represent the board position
/// For example: square a1 = (0,0), square h8 (7,7)
#[derive(Eq, Hash, Copy, Clone, PartialEq, Debug, Default)]
pub struct AbsoluteBoardPos(pub u8, pub u8);

impl Display for AbsoluteBoardPos {
    /// Formats the position in uci cell notation
    ///
    /// # Example
    /// ```
    /// use dbce::baserules::board_rep::BaseMove;
    /// use dbce::baserules::positions::AbsoluteBoardPos;
    /// use dbce::util::TryWithPanic;
    /// let first_square = "a1";
    /// let abs_pos:AbsoluteBoardPos = first_square.transform();
    /// assert_eq!(first_square, format!("{abs_pos}"));
    /// ```
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", (self.1 + b'a') as char, (self.0 + b'1') as char)
    }
}

impl TryFrom<&str> for AbsoluteBoardPos {
    type Error = AnyError;
    /// Allows loading absolute board coordinates to a BoardPos struct
    fn try_from(coord: &str) -> Result<Self, Self::Error> {
        let coord_bytes = coord.as_bytes();
        let col = Self::parse_column(coord_bytes[0])?;
        let row = Self::parse_row(coord_bytes[1])?;
        (row, col).try_into()
    }
}

impl TryFrom<(u8, u8)> for AbsoluteBoardPos {
    type Error = AnyError;
    /// Allows loading absolute board coordinates to a BoardPos struct
    fn try_from((row, col): (u8, u8)) -> Result<Self, Self::Error> {
        if (0..8).contains(&row) && (0..8).contains(&col) {
            Ok(AbsoluteBoardPos(row, col))
        } else {
            Err(format!("Row and column index out of range for {row},{col}").into())
        }
    }
}

impl TryFrom<usize> for AbsoluteBoardPos {
    type Error = AnyError;
    /// Allows loading absolute board coordinates to a BoardPos struct
    fn try_from(idx: usize) -> Result<Self, Self::Error> {
        ((idx >> 3) as u8, (idx & 0b111) as u8).try_into()
    }
}

impl ops::Add<RelativeBoardPos> for AbsoluteBoardPos {
    type Output = AbsoluteBoardPos;

    fn add(self, rhs: RelativeBoardPos) -> Self::Output {
        self.fallible_add(rhs).unwrap()
    }
}

impl AbsoluteBoardPos {
    pub fn fallible_add(self, rhs: RelativeBoardPos) -> IntResult<Self> {
        let row = self.0 as i8 + rhs.0;
        let col = self.1 as i8 + rhs.1;
        (row.try_into()?, col.try_into()?).try_into()
    }
    pub fn parse_column(col: u8) -> IntResult<u8> {
        Ok((i8::try_from(col)? - b'a' as i8).try_into()?)
    }
    pub fn parse_row(row: u8) -> IntResult<u8> {
        Ok((i8::try_from(row)? - b'1' as i8).try_into()?)
    }
}

impl ops::AddAssign<RelativeBoardPos> for AbsoluteBoardPos {
    fn add_assign(&mut self, rhs: RelativeBoardPos) {
        self.0 = (self.0 as i8 + rhs.0) as u8;
        self.1 = (self.1 as i8 + rhs.1) as u8;
    }
}

impl RelativeBoardPos {
    pub fn transform_more(
        in_iter: impl IntoIterator<Item = (i8, i8)>,
    ) -> impl IntoIterator<Item = Self> {
        in_iter.into_iter().map(|tuple| tuple.transform())
    }

    pub fn transform_to_vec(in_iter: impl IntoIterator<Item = (i8, i8)>) -> Vec<Self> {
        Self::transform_more(in_iter).into_iter().collect()
    }
}
