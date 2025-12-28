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
use actix_web::{delete, get, post, web, HttpResponse};
use serde::Deserialize;
use std::path::Path;

use crate::enemy::Enemy;
use crate::page_generation::*;
use crate::structs::*;
use crate::{shm_acc_r, shm_acc_w, webpage_path};

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
struct EnemyAddAbilityForm {
    tree: IString,
    name: IString,
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

    let enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };
    if !enemy.revealed() {
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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    enemy.set_enemy_type(form.enemy_type.clone());
    enemy.set_hp(form.hp);
    enemy.set_ac(form.ac);
    enemy.set_mov(form.mov);

    let trait_map = shm_acc_r!(TRAITS);
    let mut traits = Vec::<Trait>::new();
    for trait_name_input in &form.traits {
        let trait_name = IString::new(trait_name_input);

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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    enemy.set_skills(form.into_inner());

    enemy.save();

    HttpResponse::Ok().finish()
}

/// Macro to parse RIV effects from a vector of strings.
///
/// Returns a vector of `RivEffect`.
macro_rules! parse_riv {
    ($data_vec:expr) => {{
        let mut effect_vec = Vec::<RivEffect>::new();
        for effect in &$data_vec {
            let mut effect = IString::new(effect);

            // Check for wildcard "None" to specify no resistances:
            if $data_vec.len() == 1 && effect == "none" {
                break;
            }

            // Check that the specified effect exists:
            let riv_effects = shm_acc_r!(RIV_EFFECTS);
            if !riv_effects.contains_key(&effect) {
                // Try adding "ed" to the end, for condition effects not sent in participle form:
                let mut participle = effect.clone();
                if participle.ends_with("y") {
                    participle.pop();
                    participle += "ied";
                } else if effect.ends_with("e") {
                    participle += "d";
                } else {
                    participle += "ed";
                }

                if !riv_effects.contains_key(&participle) {
                    // Maybe the participle has to double the final consonant...
                    participle = effect.clone();
                    let consonant = &effect
                        .chars()
                        .last()
                        .expect("Effect name is empty") // Should not happen.
                        .to_string();
                    participle = participle + consonant + consonant + "ed";

                    if !riv_effects.contains_key(&participle) {
                        return HttpResponse::BadRequest().body(effect.0.clone());
                    }
                }

                // Update effect to the proper name:
                effect = participle;
            }

            effect_vec.push(shm_acc_r!(RIV_EFFECTS)[&effect].clone());
        }
        effect_vec
    }};
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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    /* Set resistances: */
    enemy.set_resistances(parse_riv!(form.resistances));

    /* Set immunities: */
    enemy.set_immunities(parse_riv!(form.immunities));

    /* Set vulnerabilities: */
    enemy.set_vulnerabilities(parse_riv!(form.vulnerabilities));

    enemy.save();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for adding abilitiy trees to an enemy.
 */
#[post("/{enemy}/ability_trees")]
async fn enemy_add_ability_trees(
    path: web::Path<String>,
    form: web::Json<Vec<IString>>,
) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    for tree_name in form.into_inner() {
        enemy.add_ability_tree(tree_name);
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
    form: web::Json<EnemyAddAbilityForm>,
) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    let tree = form.tree.clone();
    if !enemy.ability_trees().contains_key(&tree) {
        return HttpResponse::BadRequest().body(tree.0);
    }
    if let Err(e) = enemy.add_ability(&tree, form.name.clone(), form.description.clone()) {
        return HttpResponse::BadRequest().body(e);
    }

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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    let idx = form.into_inner();
    if idx > enemy.misc().len() {
        return HttpResponse::BadRequest().finish();
    }
    enemy.del_misc(idx);

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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

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
            enemy.set_img_extension(".".to_string() + extension);
            let enemy_img_path = enemy.uri_image();
            let mut out = std::fs::File::create(webpage_path!(&enemy_img_path))
                .expect(format!("Could not create file {}.", &enemy_img_path).as_str());
            reqwest::blocking::get(&image_url)
                .expect(format!("Could not download {}'s image.", enemy.name()).as_str())
                .copy_to(&mut out)
                .expect(format!("Could not save file {}.", &enemy_img_path).as_str());
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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

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

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    match info.as_str() {
        "basics" => {
            enemy.set_revealed_basics(true);
        }
        "attrs" => {
            enemy.set_revealed_attrs(true);
        }
        "skills" => {
            enemy.set_revealed_skills(true);
        }
        "riv" => {
            enemy.set_revealed_riv(true);
        }
        "all" => {
            enemy.set_revealed(true);
            enemy.set_revealed_basics(true);
            enemy.set_revealed_attrs(true);
            enemy.set_revealed_skills(true);
            enemy.set_revealed_riv(true);

            for (tree_name, ability_tree) in enemy.clone().ability_trees() {
                for ability_name in ability_tree.keys() {
                    if let Err(e) = enemy.reveal_ability(tree_name, ability_name.clone()) {
                        return HttpResponse::BadRequest().body(e);
                    }
                }
            }
        }
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
async fn reveal_enemy_ability(path: web::Path<String>, form: web::Json<IString>) -> HttpResponse {
    let data_path = Enemy::to_uri_data(&path.into_inner());

    if !Path::new(&data_path).exists() {
        return HttpResponse::NotFound().finish();
    }

    let mut enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    let ability = form.into_inner();
    let tree = enemy
        .ability_trees()
        .iter()
        .find(|(_, tree)| tree.contains_key(&ability));

    if let Some((tree_name, _)) = tree {
        if let Err(e) = enemy.reveal_ability(&tree_name.clone(), ability) {
            return HttpResponse::BadRequest().body(e);
        }
    } else {
        return HttpResponse::BadRequest().body(ability.0);
    }

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

    let enemy = match Enemy::load(data_path) {
        Ok(e) => e,
        Err(e) => return HttpResponse::BadRequest().body(e),
    };

    enemy.generate_markdown();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for adding a RivEffect.
 */
#[post("/riv")]
async fn add_riv_effect(form: web::Json<RivEffect>) -> HttpResponse {
    let effect = form.into_inner();

    let name_key = IString::new(effect.name.clone());

    if shm_acc_r!(RIV_EFFECTS).contains_key(&name_key) {
        return HttpResponse::BadRequest().body(effect.name.clone());
    }

    shm_acc_w!(RIV_EFFECTS).insert(name_key, effect.clone());
    update_riv_persistence();
    gen_riv_page();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for deleting a RivEffect.
 */
#[delete("/riv")]
async fn del_riv_effect(form: web::Json<String>) -> HttpResponse {
    let effect_name = form.into_inner();

    let name_key = IString::new(effect_name.clone());

    if !shm_acc_r!(RIV_EFFECTS).contains_key(&name_key) {
        return HttpResponse::BadRequest().body(effect_name.clone());
    }

    shm_acc_w!(RIV_EFFECTS).remove(&name_key);
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

    let name_key = IString::new(t.name.clone());

    if shm_acc_r!(TRAITS).contains_key(&name_key) {
        return HttpResponse::BadRequest().body(t.name.clone());
    }

    shm_acc_w!(TRAITS).insert(name_key, t.clone());
    update_traits_persistence();
    gen_traits_page();

    HttpResponse::Ok().finish()
}

/**
 * Endpoint for deleting a Trait.
 */
#[delete("/trait")]
async fn del_trait(form: web::Json<String>) -> HttpResponse {
    let t = form.into_inner();

    let name_key = IString::new(t.clone());

    if !shm_acc_r!(TRAITS).contains_key(&name_key) {
        return HttpResponse::BadRequest().body(t.clone());
    }

    shm_acc_w!(TRAITS).shift_remove(&name_key);
    update_traits_persistence();
    gen_traits_page();

    HttpResponse::Ok().finish()
}
