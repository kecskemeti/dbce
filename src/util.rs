/*
 *  ========================================================================
 *  DBCE chess bot, timing information supporting move deadlines
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

use std::borrow::BorrowMut;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use std::time::Duration;

pub struct DurationAverage {
    measurements: Arc<Mutex<VecDeque<Duration>>>,
    len: u32,
    avg: Duration,
    valid_avg: bool,
}

impl DurationAverage {
    pub fn new<F>(len: u32, repeatable_measurement: F) -> DurationAverage
    where
        F: Fn() -> Duration,
    {
        let measurements = Arc::new(Mutex::new(
            (1..len).map(|_| repeatable_measurement()).collect(),
        ));
        let avg = DurationAverage::calc_internal(&measurements, len);
        DurationAverage {
            measurements,
            len,
            avg,
            valid_avg: true,
        }
    }

    pub fn add(&mut self, new: Duration) {
        let mut timing_data = self.measurements.borrow_mut().lock().unwrap();
        timing_data.pop_front();
        timing_data.push_back(new);
        self.valid_avg = false;
    }

    pub fn calc_average(&mut self) -> Duration {
        if self.valid_avg {
            self.avg
        } else {
            self.valid_avg = true;
            DurationAverage::calc_internal(&self.measurements, self.len)
        }
    }

    fn calc_internal(timing_data: &Arc<Mutex<VecDeque<Duration>>>, len: u32) -> Duration {
        let timing_data = timing_data.lock().unwrap();
        let all_duration: Duration = timing_data.iter().sum();
        all_duration / len
    }
}
