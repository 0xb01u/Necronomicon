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
use std::fs;

use crate::shm_acc_r;
use crate::structs::{RivEffect, Trait, RIV_EFFECTS, TRAITS};

/* Macro to generate the correct target saving location for web pages. */
#[macro_export]
macro_rules! webpage_path {
    ($uri:expr) => {
        format!("front/{}", $uri)
    };
}

/**
 * Generates the page for the RivEffects, from the global static RIV_EFFECTS map.
 */
pub(crate) fn gen_riv_page() {
    let riv_map = shm_acc_r!(RIV_EFFECTS);
    let mut riv_effects = riv_map.values().collect::<Vec<&RivEffect>>();
    riv_effects.sort_by_key(|e| e.category.clone() + &e.name);

    let mut md = "---\nlayout: default\ntitle: RIV effects\n---\n".to_owned();
    md.push_str("# Resistance, immunity and vulnerability effects list\n\n");

    for effect in riv_effects.into_iter() {
        md.push_str(format!("- {} ({})\n", effect.name, effect.category).as_str());
    }
    md.push_str("\n");

    fs::write(webpage_path!("data/riv.md"), md).expect("Could not write data/riv.md.");
}

/**
 * Generates the page for the Traits, from the global static TRAITS map.
 */
pub(crate) fn gen_traits_page() {
    let trait_map = shm_acc_r!(TRAITS);
    // This last assignment is needed for some reason.
    // If a one-liner to generate the traits vec is attempted, the compiler complains:
    // "error[E0716]: temporary value dropped while borrowed"
    // "[the macro] creates a temporary value which is freed while still in use"
    // Seems like the implicit trait_map gets freed at the end of the one-liner,
    // but the `traits` variable still references its information later.
    let traits = trait_map.values().collect::<Vec<&Trait>>();

    let mut md = "---\nlayout: default\ntitle: Trait list\n---\n".to_owned();
    md.push_str("# Trait list\n");

    // Table of contents:
    let mut category_idx = 0;
    let mut subcategory_idx = 0;
    let mut last_category = String::new();
    let mut last_subcategory = String::new();
    for t in &traits {
        if last_category != t.category {
            last_category = t.category.clone();
            category_idx += 1;
            md.push_str(
                format!(
                    "\n{}. [{}](#{})",
                    category_idx,
                    last_category,
                    last_category.to_lowercase().replace(" ", "-")
                )
                .as_str(),
            );
            subcategory_idx = 1; // Reset subcategory index for new category.
        }
        if last_subcategory != t.subcategory {
            last_subcategory = t.subcategory.clone();
            md.push_str(
                format!(
                    " {}.{}. [{}](#{})",
                    category_idx,
                    subcategory_idx,
                    last_subcategory,
                    last_subcategory.to_lowercase().replace(" ", "-")
                )
                .as_str(),
            );
            subcategory_idx += 1;
        }
    }
    md.push_str("\n\n");

    // Trait description:
    let mut last_category = String::new();
    let mut last_subcategory = String::new();
    for t in traits {
        if last_category != t.category {
            last_category = t.category.clone();
            md.push_str(
                format!(
                    "# {} <a id=\"{}\"></a>\n\n",
                    last_category,
                    last_category.to_lowercase().replace(" ", "-")
                )
                .as_str(),
            );
        }
        if last_subcategory != t.subcategory {
            last_subcategory = t.subcategory.clone();
            md.push_str(
                format!(
                    "## {} <a id=\"{}\"></a>\n\n",
                    last_subcategory,
                    last_subcategory.to_lowercase().replace(" ", "-")
                )
                .as_str(),
            );
        }
        md.push_str(format!("### {}\n\n", t.name).as_str());
        //md.push_str(format!("- **Category:** {}\n", t.category).as_str());
        //md.push_str(format!("- **Subcategory:** {}\n\n", t.subcategory).as_str());
        md.push_str(t.description.as_str());
        md.push_str("\n\n");
    }

    fs::write(webpage_path!("data/traits.md"), md).expect("Could not write data/traits.md.");
}
