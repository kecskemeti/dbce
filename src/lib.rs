/*
 *  ========================================================================
 *  DBCE chess bot as a library
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
use crate::baserules::board::PSBoard;
use lazy_static::lazy_static;
use std::mem::size_of;
use sysinfo::{RefreshKind, System, SystemExt};

pub mod baserules;
pub mod engine;
pub mod human_facing;
pub mod util;

#[global_allocator]
static GLOBAL: tikv_jemallocator::Jemalloc = tikv_jemallocator::Jemalloc;

pub struct MemLimits {
    pub max_memory_bytes: u64,
    pub max_board_count: u32,
}

impl Default for MemLimits {
    fn default() -> Self {
        let sys = System::new_with_specifics(RefreshKind::new().with_memory());
        let available = sys.available_memory();
        MemLimits {
            max_board_count: (available as usize / size_of::<PSBoard>()).min(u32::MAX as usize)
                as u32,
            max_memory_bytes: available,
        }
    }
}

lazy_static! {
    pub static ref LIMITS: MemLimits = MemLimits::default();
}
