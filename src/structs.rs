/*
 *  Necronomicon: Web app for storing and displaying information about D&D
 *  enemies.
 *  Copyright (C) 2024-2025  Bolu <bolu@tuta.io>
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published
 *  by the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program. If not, see <https://www.gnu.org/licenses/>.
 */
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result as FmtResult},
    fs,
    hash::{Hash, Hasher},
    ops::{Add, AddAssign, Deref, DerefMut},
    sync::{LazyLock, RwLock},
};

use crate::shm_acc_r;

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Eq, PartialOrd, Serialize, Deserialize)]
pub(crate) struct IString(pub(crate) String);

impl IString {
    pub(crate) fn new<S: AsRef<str>>(s: S) -> IString {
        IString(s.as_ref().to_string())
    }
}

impl PartialEq for IString {
    fn eq(&self, other: &Self) -> bool {
        self.0.to_lowercase() == other.0.to_lowercase()
    }
}

impl PartialEq<&str> for IString {
    fn eq(&self, other: &&str) -> bool {
        self.0.to_lowercase() == other.to_lowercase()
    }
}

impl Hash for IString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_lowercase().hash(state);
    }
}

impl Display for IString {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.0)
    }
}

impl<S: AsRef<str>> Add<S> for IString {
    type Output = IString;

    fn add(self, other: S) -> IString {
        IString(self.0 + other.as_ref())
    }
}

impl<S: AsRef<str>> AddAssign<S> for IString {
    fn add_assign(&mut self, other: S) {
        self.0 += other.as_ref();
    }
}

impl Deref for IString {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

impl DerefMut for IString {
    fn deref_mut(&mut self) -> &mut String {
        &mut self.0
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct RivEffect {
    pub(crate) name: String,
    pub(crate) category: String,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Serialize, Deserialize)]
pub(crate) struct Trait {
    pub(crate) category: String,
    pub(crate) subcategory: String,
    pub(crate) name: String,
    pub(crate) description: String,
}

/* Global maps: */

// RwLock needed to make the singleton mutable;
// RwLock instead of Mutex to allow multiple concurrent readers (just in case):
pub(crate) static RIV_EFFECTS: LazyLock<RwLock<HashMap<IString, RivEffect>>> =
    LazyLock::new(|| {
        let json = fs::read_to_string("data/riv_effects.json")
            .expect("Could not read data/riv_effects.json.");
        let effects: Vec<RivEffect> = serde_json::from_str(&json)
            .expect("Could not parse data/riv_effects.json as valid JSON data.");
        let effect_map = HashMap::from_iter(
            effects
                .iter()
                .map(|e| (IString::new(e.name.clone()), e.clone())),
        );
        RwLock::new(effect_map)
    });
pub(crate) static TRAITS: LazyLock<RwLock<IndexMap<IString, Trait>>> = LazyLock::new(|| {
    let json = fs::read_to_string("data/traits.json").expect("Could not read data/traits.json.");
    let traits: Vec<Trait> =
        serde_json::from_str(&json).expect("Could not parse data/traits.json as valid JSON data.");
    let trait_map = IndexMap::from_iter(
        traits
            .iter()
            .map(|t| (IString::new(t.name.clone()), t.clone())),
    );
    RwLock::new(trait_map)
});

/**
 * Updates the persistent riv_effects.json file with the current contents of the
 * global static RIV_EFFECTS map.
 */
pub(crate) fn update_riv_persistence() {
    let json = serde_json::to_string_pretty(
        &shm_acc_r!(RIV_EFFECTS)
            .values()
            .collect::<Vec<&RivEffect>>(),
    )
    .expect("Could not serialize static RIV_EFFECTS map into JSON.");
    fs::write("data/riv_effects.json", json).expect("Could not write data/riv_effects.json.");
}

/**
 * Updates the persistent traits.json file with the current contents of the
 * global static TRAITS map.
 */
pub(crate) fn update_traits_persistence() {
    let json = serde_json::to_string_pretty(&shm_acc_r!(TRAITS).values().collect::<Vec<&Trait>>())
        .expect("Could not serialize static TRAITS map into JSON.");
    fs::write("data/traits.json", json).expect("Could not write data/traits.json.");
}

/* Macros and functions to manage global maps: */

/**
 * Macro to conveniently access a (global) static HashMap (a.k.a. "shm") for reading.
 */
#[macro_export]
macro_rules! shm_acc_r {
    ($static_hashmap:ident) => {
        (*$static_hashmap).read().expect(concat!(
            "Could not access static ",
            stringify!($static_hashmap),
            " map for reading."
        ))
    };
}

/**
 * Macro to conveniently access a (global) static HashMap (a.k.a. "shm") for writing.
 */
#[macro_export]
macro_rules! shm_acc_w {
    ($static_hashmap:ident) => {
        (*$static_hashmap).write().expect(concat!(
            "Could not access static ",
            stringify!($static_hashmap),
            " map for writing."
        ))
    };
}
