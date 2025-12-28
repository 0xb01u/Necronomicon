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
mod endpoints;
mod enemy;
mod page_generation;
mod structs;

use actix_web::{web, App, HttpServer};

use endpoints::*;
use page_generation::{gen_riv_page, gen_traits_page};

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    // Generate/update "global" pages:
    gen_riv_page();
    gen_traits_page();

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
                    .service(enemy_set_skills)
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
            .service(del_riv_effect)
            .service(add_trait)
            .service(del_trait)
    })
    .bind(("127.0.0.1", 8080))?
    .run()
    .await
}
