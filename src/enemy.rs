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
use getset::{Getters, Setters};
use indexmap::IndexMap;
use serde::{Deserialize, Serialize};
use std::{cmp, fs, path::Path};

use crate::structs::*;
use crate::webpage_path;

/* Enemy object: */
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Default, Clone, Serialize, Deserialize, Getters, Setters)]
#[getset(get = "pub(crate)")]
pub(crate) struct Enemy {
    name: String,
    #[getset(set = "pub(crate)")]
    revealed: bool,

    #[getset(set = "pub(crate)")]
    enemy_type: String,
    #[getset(set = "pub(crate)")]
    hp: i16,
    #[getset(set = "pub(crate)")]
    ac: u8,
    // TODO: Field for Martial DC.
    #[getset(set = "pub(crate)")]
    mov: u8,
    // TODO: Do not embed the entire Trait structs here.
    #[getset(set = "pub(crate)")]
    traits: Vec<Trait>,
    #[getset(set = "pub(crate)")]
    revealed_basics: bool,

    #[getset(set = "pub(crate)")]
    str: u8,
    #[getset(set = "pub(crate)")]
    dex: u8,
    #[getset(set = "pub(crate)")]
    con: u8,
    #[getset(set = "pub(crate)")]
    int: u8,
    #[getset(set = "pub(crate)")]
    wis: u8,
    #[getset(set = "pub(crate)")]
    cha: u8,
    #[getset(set = "pub(crate)")]
    str_sav: u8,
    #[getset(set = "pub(crate)")]
    dex_sav: u8,
    #[getset(set = "pub(crate)")]
    con_sav: u8,
    #[getset(set = "pub(crate)")]
    int_sav: u8,
    #[getset(set = "pub(crate)")]
    wis_sav: u8,
    #[getset(set = "pub(crate)")]
    cha_sav: u8,
    #[getset(set = "pub(crate)")]
    revealed_attrs: bool,

    #[getset(set = "pub(crate)")]
    skills: Vec<String>, // TODO: make hyper-linkable
    #[getset(set = "pub(crate)")]
    revealed_skills: bool,

    // TODO: Do not embed the entire RivEffect structs here.
    #[getset(set = "pub(crate)")]
    resistances: Vec<RivEffect>,
    #[getset(set = "pub(crate)")]
    immunities: Vec<RivEffect>,
    #[getset(set = "pub(crate)")]
    vulnerabilities: Vec<RivEffect>,
    #[getset(set = "pub(crate)")]
    revealed_riv: bool,

    ability_trees: IndexMap<IString, IndexMap<IString, (bool, String)>>,

    misc: Vec<String>,

    #[getset(set = "pub(crate)")]
    img_extension: String,
}

impl Enemy {
    pub(crate) fn new(name: String) -> Enemy {
        let mut enemy: Enemy = Default::default();
        enemy.name = name;
        enemy
    }

    pub(crate) fn add_ability_tree(&mut self, tree_name: IString) {
        self.ability_trees.insert(tree_name, IndexMap::new());
    }

    pub(crate) fn add_ability(
        &mut self,
        tree_name: &IString,
        name: IString,
        description: String,
    ) -> Result<(), String> {
        if let Some(tree) = self.ability_trees.get_mut(tree_name) {
            tree.insert(name, (false, description));
        } else {
            return Err(format!(
                "Ability tree {} not found in enemy struct.",
                tree_name
            ));
        }

        Ok(())
    }

    pub(crate) fn reveal_ability(
        &mut self,
        tree_name: &IString,
        name: IString,
    ) -> Result<(), String> {
        if let Some(tree) = self.ability_trees.get_mut(tree_name) {
            tree.entry(name)
                .and_modify(|entry| *entry = (true, entry.1.clone()));
            // entry.1 is cloned because the entry is being created in this function if it does not exist.
        } else {
            return Err(format!(
                "Ability tree {} not found in enemy struct.",
                tree_name
            ));
        }

        Ok(())
    }

    /**
     * Add a new note (misc item) to the enemy.
     */
    pub(crate) fn add_misc(&mut self, note: String) {
        self.misc.push(note);
    }

    /**
     * Delete, by index, a note from the enemy.
     */
    pub(crate) fn del_misc(&mut self, idx: usize) {
        self.misc.remove(idx - 1);
    }

    // TODO: Change (non-fs?) expects to error propagation with Results<(), Err>.
    pub(crate) fn load(path: String) -> Result<Enemy, String> {
        let Ok(json) = fs::read_to_string(&path) else {
            return Err(format!("Could not read {}", path.clone()));
        };
        let Ok(enemy) = serde_json::from_str(&json) else {
            return Err(format!(
                "Could not parse {} as valid JSON data.",
                path.clone()
            ));
        };

        Ok(enemy)
    }

    pub(crate) fn save(&self) {
        let json = serde_json::to_string_pretty(self)
            .expect(format!("Could not serialize enemy {} map into JSON.", self.name).as_str());
        fs::write(self.uri_data(), json)
            .expect(format!("Could not write {}.", self.uri_data()).as_str());
    }

    /**
     * Return the URI of this enemy's page.
     */
    pub(crate) fn uri_page(&self) -> String {
        "enemies/".to_owned() + Self::sanitize_name(&self.name).as_str() + ".md"
    }

    /**
     * Return the URI of this enemy's image.
     */
    pub(crate) fn uri_image(&self) -> String {
        "enemies/images/".to_owned()
            + Self::sanitize_name(&self.name).as_str()
            + self.img_extension.as_str()
    }

    /**
     * Return the URI of this enemy's data.
     */
    pub(crate) fn uri_data(&self) -> String {
        "data/enemies/".to_owned() + Self::sanitize_name(&self.name).as_str() + ".json"
    }

    /**
     * Return a path-friendly identifier for the enemy, based on its name.
     * Used to constuct URIs.
     */
    fn sanitize_name(name: &String) -> String {
        name.to_lowercase().replace(" ", "_").replace(",", "")
    }

    /**
     * Return the URI of the data for the enemy with the corresponding name.
     * Used to try to load a previously created enemy.
     * (The actual enemy data might not exist.)
     */
    pub(crate) fn to_uri_data(name: &String) -> String {
        "data/enemies/".to_owned() + Self::sanitize_name(name).as_str() + ".json"
    }

    /**
     * Generate the Jekyll markdown page describing this enemy.
     *
     * @return The URI to access this enemy's page in the server, if this enemy has been revealed.
     */
    pub(crate) fn generate_markdown(&self) -> Option<String> {
        if !self.revealed {
            return None;
        }

        let mut md = format!("---\nlayout: default\ntitle: {}\n---\n", self.name);

        md.push_str(format!("# {} <a id=\"main\"></a>\n\n", self.name).as_str());

        // Table of contents:
        md.push_str(
            "1. [Basic information](#basics) 1.1. [Traits](#traits)\n\
             2. [Ability modifiers](#stats)\n\
             3. [Skills](#skills)\n\
             4. [Resistances, immunities, vulnerabilities](#riv)\n\
             5. [Abilities](#abilities)\n",
        );
        if self.revealed_basics {
            for (i, (tree_name, _)) in self.ability_trees.iter().enumerate() {
                md.push_str(
                    format!(
                        " 5.{}. [{}](#{})\n",
                        i + 1,
                        tree_name,
                        tree_name.to_lowercase()
                    )
                    .as_str(),
                );
            }
        }
        md.push_str("6. [Extra notes](#misc)\n\n");

        // Image, if exists:
        if Path::new(&webpage_path!(self.uri_image())).exists() {
            md.push_str(
                format!("![{}'s picture.](../{})\n\n", self.name, self.uri_image()).as_str(),
            );
        }

        if self.revealed_basics {
            md.push_str("# Basic features <a id=\"basics\"></a>\n");
            md.push_str(format!("{}.\n\n", self.enemy_type).as_str());
            md.push_str("|Health Points|Armor Class|Movement Speed|\n|:-:|:-:|:-:|\n");
            md.push_str(format!("|{}|{}|{} ft|\n\n", self.hp, self.ac, self.mov).as_str());

            if self.traits.len() > 0 {
                md.push_str("## Traits <a id=\"traits\"></a>\n\n");
                for t in &self.traits {
                    md.push_str(
                        format!(
                            "[{}](../data/traits.html#{}), ",
                            t.name,
                            t.name.to_lowercase().replace(" ", "-")
                        )
                        .as_str(),
                    );
                }
                md.pop(); // Remove leftover space.
                md.pop(); // Remove leftover comma.
            }
            md.push_str(".\n\n");
        }

        if self.revealed_attrs {
            md.push_str("# Ability modifiers <a id=\"stats\"></a>\n\n");

            md.push_str("||Strength|Dexterity|Constitution|Intelligence|Wisdom|Charisma|\n");
            md.push_str("|-:|:-:|:-:|:-:|:-:|:-:|:-:|\n");
            md.push_str(
                format!(
                    "|**Regular**|{:+}|{:+}|{:+}|{:+}|{:+}|{:+}|\n",
                    self.str, self.dex, self.con, self.int, self.wis, self.cha
                )
                .as_str(),
            );
            md.push_str(
                format!(
                    "|**Saving**|{:+}|{:+}|{:+}|{:+}|{:+}|{:+}|\n\n",
                    self.str_sav,
                    self.dex_sav,
                    self.con_sav,
                    self.int_sav,
                    self.wis_sav,
                    self.cha_sav
                )
                .as_str(),
            );
        }

        if self.revealed_skills {
            md.push_str("# Skills <a id=skills></a>\n\n");

            for skill in &self.skills {
                md.push_str(
                    format!(
                        //"[{}](../skills/{}.html), ",
                        "{}, ",
                        skill,
                        //skill.to_lowercase().replace(" ", "-")
                    )
                    .as_str(),
                );
            }
            // Enemies with no skills are common; treat trailing spaces with care:
            if self.skills.len() > 0 {
                md.pop(); // Remove leftover space.
                md.pop(); // Remove leftover comma.

                md.push_str(".\n\n");
            }
        }

        if self.revealed_riv {
            md.push_str("# Resistances, immunities, vulnerabilities <a id=\"riv\"></a>\n\n");

            let row_amount = cmp::max(
                self.resistances.len(),
                cmp::max(self.immunities.len(), self.vulnerabilities.len()),
            );
            md.push_str("|Resistances|Immunities|Vulnerabilities|\n|:-:|:-:|:-:|\n");
            for i in 0..row_amount {
                md.push_str("|");
                if self.resistances.len() > i {
                    md.push_str(format!("{}", self.resistances[i].name).as_str());
                }
                md.push_str("|");
                if self.immunities.len() > i {
                    md.push_str(format!("{}", self.immunities[i].name).as_str());
                }
                md.push_str("|");
                if self.vulnerabilities.len() > i {
                    md.push_str(format!("{}", self.vulnerabilities[i].name).as_str());
                }
                md.push_str("|\n");
            }
            md.push_str("\n");
        }

        if self.revealed_basics {
            md.push_str("# Abilities <a id=\"abilities\"></a>\n\n");

            for (tree_name, tree_map) in &self.ability_trees {
                md.push_str(
                    format!(
                        "## {} <a id=\"{}\"></a>\n\n",
                        tree_name,
                        tree_name.to_lowercase()
                    )
                    .as_str(),
                );

                for (ability_name, (revealed, description)) in tree_map {
                    if *revealed {
                        md.push_str(format!("### {}\n\n", ability_name).as_str());
                        md.push_str(format!("{}\n\n", description).as_str());
                    } else {
                        md.push_str(format!("### _{}_\n\n", ability_name).as_str());
                    }
                }
            }
        }

        if self.misc.len() > 0 {
            let mut idx = 1;
            md.push_str("# Extra notes <a id=\"misc\"></a>\n\n");
            for e in &self.misc {
                md.push_str(format!("{}. {}\n", idx, e).as_str());
                idx += 1;
            }
            md.push_str("\n");
        }

        let uri = webpage_path!(self.uri_page());
        fs::write(&uri, md).expect(format!("Could not write {}.", &uri).as_str());
        Some(uri)
    }
}
