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

use parking_lot::RwLock;
use std::collections::VecDeque;
use std::sync::Arc;
use std::time::Duration;

#[derive(Clone)]
pub struct DurationAverage {
    measurements: Arc<RwLock<VecDeque<Duration>>>,
    len: u32,
    avg: Arc<RwLock<Option<Duration>>>,
}

impl DurationAverage {
    pub fn new<F>(len: u32, repeatable_measurement: F) -> DurationAverage
    where
        F: Fn() -> Duration,
    {
        let measurements = Arc::new(RwLock::new(
            (1..len).map(|_| repeatable_measurement()).collect(),
        ));
        let precalculated_avg = DurationAverage::calc_internal(&measurements, len);
        DurationAverage {
            measurements,
            len,
            avg: Arc::new(RwLock::new(Some(precalculated_avg))),
        }
    }

    pub fn add(&self, new: Duration) {
        {
            let mut timing_data = self.measurements.write();
            timing_data.pop_front();
            timing_data.push_back(new);
        }
        let mut avg = self.avg.write();
        *avg = None;
    }

    pub fn calc_average(&self) -> Duration {
        {
            if let Some(avg) = *self.avg.read() {
                return avg;
            }
        }
        let mut avg = self.avg.write();
        *avg = Some(DurationAverage::calc_internal(&self.measurements, self.len));
        avg.unwrap()
    }

    fn calc_internal(timing_data: &Arc<RwLock<VecDeque<Duration>>>, len: u32) -> Duration {
        let timing_data = timing_data.read();
        let all_duration: Duration = timing_data.iter().sum();
        all_duration / len
    }

    #[inline]
    pub fn len(&self) -> u32 {
        self.len
    }
}

#[derive(Clone)]
pub struct VecCache<T> {
    cache: Arc<RwLock<Vec<Vec<T>>>>,
}

impl<T> Default for VecCache<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> VecCache<T> {
    pub fn new() -> VecCache<T> {
        VecCache {
            cache: Arc::new(RwLock::new(Vec::new())),
        }
    }

    pub fn get(&self) -> Vec<T> {
        self.cache.write().pop().unwrap_or_default()
    }

    pub fn release(&self, mut cached: Vec<T>) {
        cached.clear();
        self.cache.write().push(cached)
    }
}
