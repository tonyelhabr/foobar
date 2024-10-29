#
# test_that("foobar_get_league_matches() works", {
#   testthat::skip_on_cran()
#
#   expected_league_matches_cols <- c("round", "round_name", "page_url", "id", "home", "away", "status")
#   epl_league_matches <- foobar_get_league_matches(
#     country = "ENG",
#     league_name = "Premier League"
#   )
#
#   expect_gt(nrow(epl_league_matches), 0)
#   expect_setequal(colnames(epl_league_matches), expected_league_matches_cols)
#
#   epl_league_matches <- foobar_get_league_matches(
#     league_id = 47
#   )
#
#   expect_gt(nrow(epl_league_matches), 0)
#   expect_setequal(colnames(epl_league_matches), expected_league_matches_cols)
#
#   ## test cached
#   epl_league_matches <- foobar_get_league_matches(
#     league_id = 47,
#     cached = FALSE
#   )
#
#   expect_gt(nrow(epl_league_matches), 0)
#   expect_setequal(colnames(epl_league_matches), expected_league_matches_cols)
#
#   epl_league_matches_2021 <- foobar_get_league_matches(
#     league_id = 47,
#     season = "2021/2022"
#   )
#
#   expect_gt(nrow(epl_league_matches_2021), 0)
#   expect_setequal(colnames(epl_league_matches_2021), expected_league_matches_cols)
#   expect_false(
#     epl_league_matches$page_url[1] == epl_league_matches_2021$page_url[1]
#   )
#
#   expect_error(
#     foobar_get_league_matches(
#       league_id = 47,
#       season = "2021"
#     ),
#     regexp = "`season` should be one of the following"
#   )
#
#   ## MLS is usually in-season when European leagues are out-of-season, so it's useful
#   ##   for checking that stats work in the off-season
#   mls_league_matches <- foobar_get_league_matches(
#     league_id = 130
#   )
#
#   expect_gt(nrow(mls_league_matches), 0)
#   expect_setequal(expected_league_matches_cols, colnames(mls_league_matches))
#
#   epl_ll_league_matches <- foobar_get_league_matches(
#     country =     c("ENG",            "ESP"   ),
#     league_name = c("Premier League", "LaLiga")
#   )
#
#   expect_gt(nrow(epl_ll_league_matches), 0)
#   expect_setequal(colnames(epl_ll_league_matches), expected_league_matches_cols)
#
#   epl_ll_league_matches_unnested <- epl_ll_league_matches |>
#     dplyr::select(match_id = id, home, away) |>
#     tidyr::unnest_wider(c(home, away), names_sep = "_")
#
#   expect_gt(nrow(epl_ll_league_matches_unnested), 0)
#   expect_setequal(
#     colnames(epl_ll_league_matches_unnested),
#     c("match_id", "home_name", "home_shortName", "home_id", "away_name", "away_shortName", "away_id")
#   )
#
#   # doesn't exist
#   expect_error(
#     foobar_get_league_matches(
#       league_id = 1
#     )
#   )
#
#   # must also provide league_name
#   expect_error(
#     foobar_get_league_matches(
#       country = "ENG"
#     )
#   )
#
#   # must also provide country
#   expect_error(
#     foobar_get_league_matches(
#       league_name = "Premier League"
#     )
#   )
#
#   # mis-specified league_name
#   expect_error(
#     foobar_get_league_matches(
#       country = "ESP",
#       league_name = "La Liga"
#     )
#   )
#
#   # "La Liga" should be "LaLiga". the function will work just for the Premier League
#   expect_warning(
#     foobar_get_league_matches(
#       country =     c("ENG",            "ESP"   ),
#       league_name = c("Premier League", "La Liga")
#     )
#   )
#
#   # "SPA" should be "ESP". the function will work just for the Premier League
#   expect_warning(
#     foobar_get_league_matches(
#       country =     c("ENG",            "SPA"   ),
#       league_name = c("Premier League", "LaLiga")
#     )
#   )
# })
#
# test_that("foobar_get_league_tables() works", {
#   testthat::skip_on_cran()
#
#   expected_domestic_league_table_cols <- c("league_id", "page_url", "table_type", "table_name", "table_short_name", "table_id", "table_page_url", "table_deduction", "table_ongoing", "table_played", "table_wins", "table_draws", "table_losses", "table_scores_str", "table_goal_con_diff", "table_pts", "table_idx", "table_qual_color")
#   expect_table_cols <- function(df, exepcted_cols) {
#     expect_true(all(exepcted_cols %in% colnames(df)))
#   }
#   epl_league_table <- foobar_get_league_tables(
#     country = "ENG",
#     league_name = "Premier League"
#   )
#
#   ## should be 20 teams x 3 table types = 60
#   expect_gt(nrow(epl_league_table), 0)
#   expect_table_cols(epl_league_table, expected_domestic_league_table_cols)
#
#   epl_league_table <- foobar_get_league_tables(
#     league_id = 47
#   )
#
#   expect_gt(nrow(epl_league_table), 0)
#   expect_table_cols(epl_league_table, expected_domestic_league_table_cols)
#
#   ## past season
#   epl_league_table_2021 <- foobar_get_league_tables(
#     league_id = 47,
#     season = "2021/2022"
#   )
#
#   expect_gt(nrow(epl_league_table_2021), 0)
#   expect_table_cols(epl_league_table, expected_domestic_league_table_cols)
#   expect_false(
#     all(epl_league_table_2021$table_scores_str[1:20] == epl_league_table$table_scores_str[1:20])
#   )
#
#   expect_error(
#     foobar_get_league_tables(
#       league_id = 47,
#       season = "2021"
#     ),
#     regexp = "`season` should be one of the following"
#   )
#
#   ## see not about MLS from before
#   mls_league_table <- foobar_get_league_tables(
#     league_id = 130
#   )
#
#   expect_gt(nrow(mls_league_table), 0)
#   ## MLS typically has 4 extra columns, sometimes 5. Don't check for the fifth, "ongoing", since it depends on the time of year.
#   expect_table_cols(
#     mls_league_table,
#     c(expected_domestic_league_table_cols, "ccode", "group_id", "group_page_url", "group_name")
#   )
#
#   epl_ll_league_tables <- foobar_get_league_tables(
#     country =     c("ENG",            "ESP"   ),
#     league_name = c("Premier League", "LaLiga")
#   )
#
#   ## should be 2 leagues x 20 teams x 3 table types = 120
#   expect_gt(nrow(epl_ll_league_tables), 0)
#   expect_table_cols(epl_ll_league_tables, expected_domestic_league_table_cols)
#
#   table_types <- dplyr::distinct(epl_ll_league_tables, table_type)
#   expect_true(all(c("all", "home", "away") %in% table_types$table_type))
#
#   ## non-domestic league
#   ## Can only check on CL after group stages and briefly after the final.
#   m <- lubridate::month(Sys.Date())
#   if(m >= 1 && m <= 5) {
#     expected_international_league_table_cols <- c("league_id", "page_url", "ccode", "group_id", "group_page_url", "group_name", "ongoing", "table_type", "table_name", "table_short_name", "table_id", "table_page_url", "table_deduction", "table_ongoing", "table_played", "table_wins", "table_draws", "table_losses", "table_scores_str", "table_goal_con_diff", "table_pts", "table_idx", "table_qual_color")
#
#     cl_league_table <- foobar_get_league_tables(
#       country =     "INT",
#       league_name = "Champions League"
#     )
#
#     ## should be 32 teams x 3 table types = 96
#     expect_gt(nrow(cl_league_table), 0)
#     expect_table_cols(cl_league_table, expected_international_league_table_cols)
#   }
# })
