/*
 *  ========================================================================
 *  DBCE chess bot, caching + timing information supporting move deadlines
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
 *  (C) Copyright 2022, Gabor Kecskemeti
 */

use std::collections::VecDeque;
use std::error::Error;
use std::fmt::Debug;
use std::time::Duration;

pub type AnyError = Box<dyn Error>;
pub type IntResult<T> = Result<T, AnyError>;
pub type EmptyResult = IntResult<()>;

pub trait TryWithPanic<T: Debug> {
    fn transform(self) -> T;
}

impl<T, U> TryWithPanic<U> for T
where
    U: TryFrom<T, Error = AnyError> + Debug,
{
    fn transform(self) -> U {
        self.try_into().unwrap()
    }
}

#[derive(Clone)]
pub struct DurationAverage(VecDeque<Duration>);

impl DurationAverage {
    pub fn new<F>(len: u32, repeatable_measurement: F) -> Self
    where
        F: Fn() -> Duration,
    {
        assert_ne!(len, 0);
        Self((1..len).map(|_| repeatable_measurement()).collect())
    }

    pub fn add(&mut self, new: Duration) {
        self.0.pop_front();
        self.0.push_back(new);
    }

    pub fn calc_average(&self) -> Duration {
        let all_duration: Duration = self.0.iter().sum();
        all_duration / self.0.len() as u32
    }

    #[inline]
    pub fn length(&self) -> usize {
        self.0.len()
    }
}
