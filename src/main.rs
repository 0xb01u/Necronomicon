/*
 *  Necronomicon: Web app for storing and displaying information about D&D
 *  enemies.
 *  Copyright (C) 2024  Bolu <bolu@tuta.io>
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
use std::cmp;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::sync::{LazyLock, RwLock};
extern crate reqwest;

use actix_web::{delete, get, post, web, App, HttpResponse, HttpServer};
use getset::{Getters, Setters};
use serde::{Deserialize, Serialize};

/* Macro to generate the correct target saving location for web pages. */
#[macro_export]
macro_rules! webpage_path {
    ($uri:expr) => {
        format!("front/{}", $uri)
    };
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Serialize, Deserialize)]
struct RivEffect {
    name: String,
    category: String,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone, Serialize, Deserialize)]
struct Trait {
    category: String,
    subcategory: String,
    name: String,
    description: String,
}

/* Enemy object: */
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Default, Serialize, Deserialize, Getters, Setters)]
struct Enemy {
    #[getset(get)]
    name: String,
    #[getset(get, set)]
    revealed: bool,

    #[getset(get, set)]
    enemy_type: String,
    #[getset(get, set)]
    hp: i16,
    #[getset(get, set)]
    ac: u8,
    #[getset(get, set)]
    mov: u8,
    #[getset(get, set)]
    traits: Vec<Trait>,
    #[getset(get, set)]
    revealed_basics: bool,

    #[getset(get, set)]
    str: u8,
    #[getset(get, set)]
    dex: u8,
    #[getset(get, set)]
    con: u8,
    #[getset(get, set)]
    int: u8,
    #[getset(get, set)]
    wis: u8,
    #[getset(get, set)]
    cha: u8,
    #[getset(get, set)]
    str_sav: u8,
    #[getset(get, set)]
    dex_sav: u8,
    #[getset(get, set)]
    con_sav: u8,
    #[getset(get, set)]
    int_sav: u8,
    #[getset(get, set)]
    wis_sav: u8,
    #[getset(get, set)]
    cha_sav: u8,
    #[getset(get, set)]
    revealed_attrs: bool,

    #[getset(get, set)]
    skills: Vec<String>, // TODO: make hyper-linkable
    #[getset(get, set)]
    revealed_skills: bool,

    #[getset(get, set)]
    resistances: Vec<RivEffect>,
    #[getset(get, set)]
    immunities: Vec<RivEffect>,
    #[getset(get, set)]
    vulnerabilities: Vec<RivEffect>,
    #[getset(get, set)]
    revealed_riv: bool,

    #[getset(get)]
    ability_trees: HashMap<String, HashMap<String, (bool, String)>>,

    #[getset(get)]
    misc: Vec<String>,

    img_extension: String,
}

impl Enemy {
    fn new(name: String) -> Enemy {
        let mut enemy: Enemy = Default::default();
        enemy.name = name;
        enemy
    }

    // Maybe, proper error propagation should be used instead of .expect()s.
    // However, the logic would get considerably more complicated, and the benefits in this case
    // would be minimal.

    fn add_ability_tree(&mut self, tree_name: String) {
        self.ability_trees.insert(tree_name, HashMap::new());
    }

    fn add_ability(&mut self, tree_name: &String, name: String, description: String) {
        self.ability_trees
            .get_mut(tree_name)
            .expect(format!("Ability tree {} not found in enemy struct.", tree_name).as_str())
            .insert(name, (false, description));
    }

    fn change_ability_description(
        &mut self,
        tree_name: &String,
        name: &String,
        description: String,
    ) {
        self.ability_trees
            .get_mut(tree_name)
            .expect(format!("Ability tree {} not found in enemy struct.", tree_name).as_str())
            .entry(name.clone()) // Cloned because the entry is created if it does not exist.
            .and_modify(|desc| *desc = (desc.0, description));
    }

    fn reveal_ability(&mut self, tree_name: &String, name: &String) {
        self.ability_trees
            .get_mut(tree_name)
            .expect(format!("Ability tree {} not found in enemy struct.", tree_name).as_str())
            .entry(name.clone()) // Cloned because the entry is created if it does not exist.
            .and_modify(|desc| *desc = (true, desc.1.clone()));
    }

    /**
     * Add a new note (misc item) to the enemy.
     */
    fn add_misc(&mut self, note: String) {
        self.misc.push(note);
    }

    /**
     * Remove, by index, a note from the enemy.
     */
    fn remove_misc(&mut self, idx: usize) {
        self.misc.remove(idx - 1);
    }

    fn load(path: String) -> Enemy {
        let json =
            fs::read_to_string(&path).expect(format!("Could not read {}", path.clone()).as_str());
        serde_json::from_str(&json)
            .expect(format!("Could not parse {} as valid JSON data.", path).as_str())
    }

    fn save(&self) {
        let json = serde_json::to_string_pretty(self)
            .expect(format!("Could not serialize enemy {} map into JSON.", self.name).as_str());
        fs::write(self.uri_data(), json)
            .expect(format!("Could not write {}.", self.uri_data()).as_str());
    }

    /**
     * Return the URI of this enemy's page.
     */
    fn uri_page(&self) -> String {
        "enemies/".to_owned() + Self::get_id(&self.name).as_str() + ".md"
    }

    /**
     * Return the URI of this enemy's image.
     */
    fn uri_image(&self) -> String {
        "enemies/images/".to_owned()
            + Self::get_id(&self.name).as_str()
            + self.img_extension.as_str()
    }

    /**
     * Return the URI of this enemy's data.
     */
    fn uri_data(&self) -> String {
        "data/enemies/".to_owned() + Self::get_id(&self.name).as_str() + ".json"
    }

    /**
     * Return a path-friendly identifier for the enemy, based on its name.
     * Used to constuct URIs.
     */
    fn get_id(name: &String) -> String {
        name.to_lowercase().replace(" ", "_")
    }

    /**
     * Return the URI of the data for the enemy with the corresponding name.
     * Used to try to load a previously created enemy.
     * (The actual enemy data might not exist.)
     */
    fn to_uri_data(name: &String) -> String {
        "data/enemies/".to_owned() + Self::get_id(name).as_str() + ".json"
    }

    /**
     * Generate the Jekyll markdown page describing this enemy.
     *
     * @return The URI to access this enemy's page in the server, if this enemy has been revealed.
     */
    fn generate_markdown(&self) -> Option<String> {
        if !self.revealed {
            return None;
        }

        let mut md = format!("---\nlayout: default\ntitle: {}\n---\n", self.name);

        md.push_str(format!("# {} <a id=\"main\"></a>\n\n", self.name).as_str());

        // Image, if exists:
        if !Path::new(&self.uri_image()).exists() {
            md.push_str(
                format!("![{}'s picture.](../{})\n\n", self.name, self.uri_image()).as_str(),
            );
        }

        // Table of contents:
        md.push_str(
            "1. [Basic information](#basics)\n\
             \t1. [Traits](#traits)\n\
             2. [Ability modifiers](#stats)\n\
             2. [Skills](#skills)\n\
             4. [Resistances, immunities, vulnerabilities](#riv)\n\
             5. [Abilities](#abilities)\n\
             6. [Extra notes](#misc)\n\n",
        );

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

            md.push_str("|Strength|Dexterity|Constitution|Intelligence|Wisdom|Charisma|\n");
            md.push_str("|:-:|:-:|:-:|:-:|:-:|:-:|\n");
            md.push_str(
                format!(
                    "|{:+}|{:+}|{:+}|{:+}|{:+}|{:+}|\n",
                    self.str, self.dex, self.con, self.int, self.wis, self.cha
                )
                .as_str(),
            );
            md.push_str(
                format!(
                    "|{:+}|{:+}|{:+}|{:+}|{:+}|{:+}|\n\n",
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
                        "[{}](../skills/{}.html), ",
                        skill,
                        skill.to_lowercase().replace(" ", "-")
                    )
                    .as_str(),
                );
            }
            md.pop(); // Remove leftover space.
            md.pop(); // Remove leftover comma.

            md.push_str("\n\n");
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
                md.push_str(format!("## {}\n\n", tree_name).as_str());

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

/* Global maps: */

// RwLock needed to make the singleton mutable;
// RwLock instead of Mutex to allow multiple concurrent readers (just in case):
static RIV_EFFECTS: LazyLock<RwLock<HashMap<String, RivEffect>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));
static TRAITS: LazyLock<RwLock<HashMap<String, Trait>>> =
    LazyLock::new(|| RwLock::new(HashMap::new()));

/* Macros and functions to manage global maps: */

/**
 * Macro to conveniently access a (global)static HashMap (a.k.a. "shm") for reading.
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

/**
 * Populates the global static RIV_EFFECTS map from the riv_effects.json persistent file.
 */
fn populate_riv_effects() {
    let json =
        fs::read_to_string("data/riv_effects.json").expect("Could not read data/riv_effects.json.");
    let riv_effects: Vec<RivEffect> = serde_json::from_str(&json)
        .expect("Could not parse data/riv_effects.json as valid JSON data.");

    let mut static_riv_effects = shm_acc_w!(RIV_EFFECTS);
    static_riv_effects.drain(); // Remove all previous keys.

    for e in riv_effects.iter() {
        static_riv_effects.insert(e.name.clone().to_lowercase(), e.clone());
    }
}

/**
 * Generates the page for the RivEffects, from the global static RIV_EFFECTS map.
 */
fn gen_riv_page() {
    let riv_effects = shm_acc_r!(RIV_EFFECTS);

    let mut md = "---\nlayout: default\ntitle: RIV effects\n---\n".to_owned();
    md.push_str("# Resistance, immunity and vulnerability effects list\n\n");

    for effect in riv_effects.values() {
        md.push_str(format!("- {} ({})\n", effect.name, effect.category).as_str());
    }
    md.push_str("\n");

    fs::write(webpage_path!("data/riv.md"), md).expect("Could not write data/riv.md.");
}

/**
 * Updates the persistent riv_effects.json file with the current contents of the
 * global static RIV_EFFECTS map.
 */
fn update_riv_persistence() {
    let json = serde_json::to_string_pretty(
        &shm_acc_r!(RIV_EFFECTS)
            .values()
            .collect::<Vec<&RivEffect>>(),
    )
    .expect("Could not serialize static RIV_EFFECTS map into JSON.");
    fs::write("data/riv_effects.json", json).expect("Could not write data/riv_effects.json.");
}

/**
 * Populates the global static TRAITS map from the traits.json persistent file.
 */
fn populate_traits() {
    let json = fs::read_to_string("data/traits.json").expect("Could not read data/traits.json.");
    let traits: Vec<Trait> =
        serde_json::from_str(&json).expect("Could not parse data/traits.json as valid JSON data.");

    let mut static_traits = shm_acc_w!(TRAITS);
    static_traits.drain(); // Remove all previous keys.

    for t in traits.iter() {
        static_traits.insert(t.name.clone().to_lowercase(), t.clone());
    }
}

/**
 * Generates the page for the Traits, from the global static TRAITS map.
 */
fn gen_traits_page() {
    let trait_map = shm_acc_r!(TRAITS);
    // This last assignment is needed for some reason.
    // If a one-liner to generate the traits vec is attempted, the compiler complains:
    // "error[E0716]: temporary value dropped while borrowed"
    // "[the macro] creates a temporary value which is freed while still in use"
    // Seems like the implicit trait_map gets freed at the end of the one-liner,
    // but the `traits` variable still references its information later.
    let mut traits = trait_map.values().collect::<Vec<&Trait>>();
    // Sort for better human searching:
    traits.sort_by_key(|e| e.name.clone());

    let mut md = "---\nlayout: default\ntitle: Trait list\n---\n".to_owned();
    md.push_str("# Trait list\n\n");

    let mut idx = 1;
    for t in &traits {
        md.push_str(
            format!(
                "{}. [{}](#{})\n",
                idx,
                t.name,
                t.name.to_lowercase().replace(" ", "-")
            )
            .as_str(),
        );
        idx += 1;
    }
    md.push_str("\n");

    for t in traits {
        md.push_str(
            format!(
                "## {} <a id=\"{}\"></a>\n\n",
                t.name,
                t.name.to_lowercase().replace(" ", "-").as_str()
            )
            .as_str(),
        );
        md.push_str(format!("- **Category:** {}\n", t.category).as_str());
        md.push_str(format!("- **Subcategory:** {}\n\n", t.subcategory).as_str());
        md.push_str(t.description.as_str());
        md.push_str("\n\n");
    }

    fs::write(webpage_path!("data/traits.md"), md).expect("Could not write data/traits.md.");
}

/**
 * Updates the persistent traits.json file with the current contents of the
 * global static TRAITS map.
 */
fn update_traits_persistence() {
    let json = serde_json::to_string_pretty(&shm_acc_r!(TRAITS).values().collect::<Vec<&Trait>>())
        .expect("Could not serialize static TRAITS map into JSON.");
    fs::write("data/traits.json", json).expect("Could not write data/traits.json.");
}

/* Structures for the endpoint forms: */

#[derive(Deserialize)]
struct EnemyBasicsForm {
    enemy_type: String,
    hp: i16,
    ac: u8,
    mov: u8,
    traits: Vec<String>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Deserialize)]
struct EnemyAttributesForm {
    str: u8,
    dex: u8,
    con: u8,
    int: u8,
    wis: u8,
    cha: u8,
    str_sav: u8,
    dex_sav: u8,
    con_sav: u8,
    int_sav: u8,
    wis_sav: u8,
    cha_sav: u8,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Deserialize)]
struct EnemyRIVForm {
    resistances: Vec<String>,
    immunities: Vec<String>,
    vulnerabilities: Vec<String>,
}

#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Deserialize)]
struct EnemyAbilityForm {
    tree: String,
    name: String,
    description: String,
}

/* Endpoints: */

/**
 * Endpoint for creating a new enemy.
 */
#[post("/")]
async fn create_enemy(form: web::Json<String>) -> HttpResponse {
    let name = form.into_inner();

    if Path::new(&Enemy::to_uri_data(&name)).exists() {
        return HttpResponse::Forbidden().finish();
    }

    let enemy = Enemy::new(name);
    enemy.save();

    HttpResponse::Created().finish()
}

/**
 * Endpoint for retrieving the webpage for an enemy.
 */
#[get("/")]
async fn retrieve_enemy(form: web::Json<String>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&form);

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let enemy = Enemy::load(data_path);
    if !enemy.revealed {
        // Enemy not revealed yet, return Forbidden:
        return HttpResponse::Forbidden().finish();
    }

    HttpResponse::Ok().body(enemy.uri_page().replace("md", "html"))
}

/**
 * Endpoint for modifying the basic information of an enemy.
 */
#[post("/{enemy}/basics")]
async fn enemy_set_basics(
    path: web::Path<String>,
    form: web::Json<EnemyBasicsForm>,
) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    enemy.set_enemy_type(form.enemy_type.clone());
    enemy.set_hp(form.hp);
    enemy.set_ac(form.ac);
    enemy.set_mov(form.mov);

    let trait_map = shm_acc_r!(TRAITS);
    let mut traits = Vec::<Trait>::new();
    for trait_name_input in &form.traits {
        let trait_name = trait_name_input.to_lowercase();

        // Check that the specified trait exists:
        if !trait_map.contains_key(&trait_name) {
            return HttpResponse::BadRequest().body(trait_name_input.clone());
        }

        traits.push(trait_map[&trait_name].clone());
    }
    enemy.set_traits(traits);

    enemy.save();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for modifying the attributes (i.e. ability modifiers) of an enemy.
 */
#[post("/{enemy}/attributes")]
async fn enemy_set_attrs(
    path: web::Path<String>,
    form: web::Json<EnemyAttributesForm>,
) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    enemy.set_str(form.str);
    enemy.set_dex(form.dex);
    enemy.set_con(form.con);
    enemy.set_int(form.int);
    enemy.set_wis(form.wis);
    enemy.set_cha(form.cha);

    enemy.set_str_sav(form.str_sav);
    enemy.set_dex_sav(form.dex_sav);
    enemy.set_con_sav(form.con_sav);
    enemy.set_int_sav(form.int_sav);
    enemy.set_wis_sav(form.wis_sav);
    enemy.set_cha_sav(form.cha_sav);

    enemy.save();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for modifying the skills of an enemy.
 */
#[post("/{enemy}/skills")]
async fn enemy_set_skills(path: web::Path<String>, form: web::Json<Vec<String>>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    enemy.set_skills(form.into_inner());

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for modifying the resistances, immunities and vulnerabilities of an enemy.
 */
#[post("/{enemy}/riv")]
async fn enemy_set_riv(path: web::Path<String>, form: web::Json<EnemyRIVForm>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    let riv_map = shm_acc_r!(RIV_EFFECTS);

    /* Set resistances: */
    let mut resistances = Vec::<RivEffect>::new();
    for resistance_name_input in &form.resistances {
        let resistance_name = resistance_name_input.to_lowercase();

        // Check that the specified resistance exists:
        if !riv_map.contains_key(&resistance_name) {
            return HttpResponse::BadRequest().body(resistance_name_input.clone());
        }

        resistances.push(riv_map[&resistance_name].clone());
    }
    enemy.set_resistances(resistances);

    /* Set immunities: */
    let mut immunities = Vec::<RivEffect>::new();
    for immunity_name_input in &form.immunities {
        let immunity_name = immunity_name_input.to_lowercase();

        // Check that the specified immunity exists:
        if !riv_map.contains_key(&immunity_name) {
            return HttpResponse::BadRequest().body(immunity_name_input.clone());
        }

        immunities.push(riv_map[&immunity_name].clone());
    }
    enemy.set_immunities(immunities);

    /* Set vulnerabilities: */
    let mut vulnerabilities = Vec::<RivEffect>::new();
    for vulnerability_name_input in &form.vulnerabilities {
        let vulnerability_name = vulnerability_name_input.to_lowercase();

        // Check that the specified vulnerability exists:
        if !riv_map.contains_key(&vulnerability_name) {
            return HttpResponse::BadRequest().body(vulnerability_name_input.clone());
        }

        vulnerabilities.push(riv_map[&vulnerability_name].clone());
    }
    enemy.set_vulnerabilities(vulnerabilities);

    enemy.save();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for adding abilitiy trees to an enemy.
 */
#[post("/{enemy}/ability_trees")]
async fn enemy_add_ability_trees(
    path: web::Path<String>,
    form: web::Json<Vec<String>>,
) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    for tree_name in form.into_inner() {
        enemy.add_ability_tree(tree_name.clone());
    }

    enemy.save();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for adding abilities to an enemy.
 */
#[post("/{enemy}/ability")]
async fn enemy_add_ability(
    path: web::Path<String>,
    form: web::Json<EnemyAbilityForm>,
) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    let tree = &form.tree;
    if !enemy.ability_trees().contains_key(tree) {
        return HttpResponse::BadRequest().body(tree.clone());
    }
    enemy.add_ability(tree, form.name.clone(), form.description.clone());

    enemy.save();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for adding a note to an enenmy.
 */
#[post("/{enemy}/note")]
async fn enemy_add_note(path: web::Path<String>, form: web::Json<String>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    enemy.add_misc(form.into_inner().clone());

    enemy.save();
    enemy.generate_markdown();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for removing a note from an enenmy.
 */
#[delete("/{enemy}/note")]
async fn enemy_del_note(path: web::Path<String>, form: web::Json<usize>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    let idx = form.into_inner();
    if idx >= enemy.misc().len() {
        return HttpResponse::BadRequest().finish();
    }
    enemy.remove_misc(idx);

    enemy.save();
    enemy.generate_markdown();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for setting the image for an enemy.
 */
#[post("/{enemy}/image")]
async fn enemy_set_image(path: web::Path<String>, form: web::Json<String>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    let enemy_img_path = enemy.uri_image();
    let image_url = form.into_inner();

    let extension = image_url
        .split('.')
        .next_back()
        .unwrap() // Should not give an error.
        .split('?') // Trim extra URL data.
        .next()
        .unwrap(); // Should not give an error.
    match extension {
        "bmp" | "png" | "jpeg" | "jpg" | "avif" => {
            // Save image to file:
            let mut out = std::fs::File::create(webpage_path!(&enemy_img_path))
                .expect(format!("Could not create file {}.", &enemy_img_path).as_str());
            reqwest::blocking::get(&image_url)
                .expect(format!("Could not download {}'s image.", enemy.name).as_str())
                .copy_to(&mut out)
                .expect(format!("Could not save file {}.", &enemy_img_path).as_str());
            enemy.img_extension = ".".to_string() + extension;
        }
        _ => {
            return HttpResponse::BadRequest().body("Unknown image type");
        }
    };

    enemy.save();
    enemy.generate_markdown();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for revealing an enemy.
 */
#[post("/{enemy}/reveal")]
async fn reveal_enemy(path: web::Path<String>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    enemy.set_revealed(true);

    enemy.save();
    enemy.generate_markdown();

    HttpResponse::Created().body(enemy.uri_page().replace("md", "html"))
}

/**
 * Endpoint for revealing an enemy's information.
 */
#[post("/{enemy}/reveal/{info}")]
async fn reveal_enemy_info(path: web::Path<(String, String)>) -> HttpResponse {
    let (name, info) = path.into_inner();
    let data_path = Enemy::to_uri_data(&name);

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    match info.as_str() {
        "basics" => enemy.set_revealed_basics(true),
        "attrs" => enemy.set_revealed_attrs(true),
        "skills" => enemy.set_revealed_skills(true),
        "riv" => enemy.set_revealed_riv(true),
        _ => return HttpResponse::BadRequest().body(info),
    };

    enemy.save();
    enemy.generate_markdown();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for revealing an enemy's abilities.
 */
#[post("/{enemy}/reveal/ability")]
async fn reveal_enemy_ability(
    path: web::Path<String>,
    form: web::Json<EnemyAbilityForm>,
) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = Enemy::load(data_path);

    let tree = &form.tree;
    if !enemy.ability_trees().contains_key(tree) {
        return HttpResponse::BadRequest().body(tree.clone());
    }

    let ability = &form.name;
    if !enemy.ability_trees()[tree].contains_key(ability) {
        return HttpResponse::BadRequest().body(ability.clone());
    }

    enemy.reveal_ability(tree, ability);

    enemy.save();
    enemy.generate_markdown();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for refreshing (regenerate) an enemy's page.
 */
#[post("/{enemy}/refresh")]
async fn refresh_enemy_page(path: web::Path<String>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let enemy = Enemy::load(data_path);

    enemy.generate_markdown();

    HttpResponse::Ok().finish()
}

// TODO: Sanitize RivEffect and Trait names?
/**
 * Endpoint for adding a RivEffect.
 */
#[post("/riv")]
async fn add_riv_effect(form: web::Json<RivEffect>) -> HttpResponse {
    let effect = form.into_inner();

    if shm_acc_r!(RIV_EFFECTS).contains_key(&effect.name) {
        return HttpResponse::BadRequest().body(effect.name);
    }

    shm_acc_w!(RIV_EFFECTS).insert(effect.name.clone(), effect.clone());
    update_riv_persistence();
    gen_riv_page();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for adding a Trait.
 */
#[post("/trait")]
async fn add_trait(form: web::Json<Trait>) -> HttpResponse {
    let t = form.into_inner();

    if shm_acc_r!(TRAITS).contains_key(&t.name) {
        return HttpResponse::BadRequest().body(t.name);
    }

    shm_acc_w!(TRAITS).insert(t.name.clone(), t.clone());
    update_traits_persistence();
    gen_traits_page();

    HttpResponse::Ok().finish()
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Initialize global maps:
    populate_traits();
    gen_traits_page();
    populate_riv_effects();
    gen_riv_page();

    // Create and run server:
    println!("Necronomicon listening on 127.0.0.1:8080");
    HttpServer::new(|| {
        App::new()
            .service(
                // Services for all enemy-related stuff:
                web::scope("/enemy")
                    .service(create_enemy)
                    .service(retrieve_enemy)
                    .service(enemy_set_basics)
                    .service(enemy_set_attrs)
                    .service(enemy_set_riv)
                    .service(enemy_add_ability_trees)
                    .service(enemy_add_ability)
                    .service(enemy_add_note)
                    .service(enemy_del_note)
                    .service(enemy_set_image)
                    .service(reveal_enemy)
                    .service(reveal_enemy_ability) // This must come before reveal_enemy_info
                    // because their paths overlap.
                    .service(reveal_enemy_info)
                    .service(refresh_enemy_page),
            )
            .service(add_riv_effect)
            .service(add_trait)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
