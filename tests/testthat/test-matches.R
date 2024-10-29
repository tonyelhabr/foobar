# test_that("foobar_get_matches_by_date() works", {
#   testthat::skip_on_cran()
#
#   expected_matches_by_date_cols <- c("away_id", "away_name", "away_score", "ccode", "home_id", "home_name", "home_score", "id", "match_id", "match_league_id")
#
#   td <- Sys.Date()
#   results <- foobar_get_matches_by_date(date = as.character(td - c(7, 8)))
#   expect_gt(nrow(results), 0)
#   ## There have been issues where the columns are not in the same exact order depending on the day, so rely on sort
#   expect_true(all(expected_matches_by_date_cols %in% colnames(results)))
#
#   future_results <- foobar_get_matches_by_date(date = as.character(td + c(1, 2)))
#   expect_gt(nrow(future_results), 0)
#   expect_true(all(expected_matches_by_date_cols %in% colnames(future_results)))
# })
#
# test_that("foobar_get_match_info() works", {
#   testthat::skip_on_cran()
#
#   expected_match_info_cols <- c("match_id", "match_round", "league_id", "league_name", "league_round_name", "parent_league_id", "parent_league_season", "match_time_utc", "home_team_id", "home_team", "away_team_id", "away_team", "match_date_utc_time", "tournament_id", "tournament_link", "tournament_league_name", "tournament_round", "tournament_selected_season", "tournament_is_current_season", "stadium_name", "stadium_city", "stadium_country", "stadium_lat", "stadium_long", "referee_img_url", "referee_text", "referee_country", "attendance")
#   expect_match_info_cols <- function(df) {
#     expect_true(all(expected_match_info_cols %in% colnames(df)))
#   }
#   match_info <- foobar_get_match_info(c(3609987, 3609979))
#
#   expect_gt(nrow(match_info), 0)
#   expect_match_info_cols(match_info)
#
#   ## non-domestic match
#   match_info <- foobar_get_match_info(3846342)
#   expect_gt(nrow(match_info), 0)
#   expect_match_info_cols(match_info)
# })
#
# test_that("foobar_get_match_team_stats() works", {
#   testthat::skip_on_cran()
#
#   expected_match_team_stats_cols <- c("match_id", "match_round", "league_id", "league_name", "league_round_name", "parent_league_id", "parent_league_season", "match_time_utc", "home_team_id", "home_team", "away_team_id", "away_team", "title", "stats_title", "home_value", "away_value", "stats_type", "stats_highlighted")
#   expect_match_team_stats_cols <- function(df) {
#     expect_true(all(expected_match_team_stats_cols %in% colnames(df)))
#   }
#   match_team_stats <- foobar_get_match_team_stats(c(3609987, 3609979))
#
#   expect_gt(nrow(match_team_stats), 0)
#   expect_match_team_stats_cols(match_team_stats)
#
#   ## non-domestic match
#   match_team_stats <- foobar_get_match_team_stats(3846342)
#   expect_gt(nrow(match_team_stats), 0)
#   expect_match_team_stats_cols(match_team_stats)
# })
#
# test_that("foobar_get_match_details() works", {
#   testthat::skip_on_cran()
#
#   expected_match_detail_nonshot_cols <- c("match_id", "match_round", "league_id", "league_name", "league_round_name", "parent_league_id", "parent_league_season", "match_time_utc", "home_team_id", "home_team", "away_team_id", "away_team")
#   expected_match_detail_shot_cols <- c("id", "event_type", "team_id", "player_id", "player_name", "x", "y", "min", "min_added", "is_blocked", "is_on_target", "blocked_x", "blocked_y", "goal_crossed_y", "goal_crossed_z", "expected_goals", "expected_goals_on_target", "shot_type", "situation", "period", "is_own_goal", "on_goal_shot_x", "on_goal_shot_y", "on_goal_shot_zoom_ratio")
#
#   expect_match_detail_nonshot_cols <- function(df) {
#     expect_true(all(expected_match_detail_nonshot_cols %in% colnames(df)))
#   }
#
#   expect_match_detail_shot_cols <- function(df) {
#     expect_true(all(expected_match_detail_shot_cols %in% colnames(df)))
#   }
#
#   expected_match_detail_cols <- c(expected_match_detail_nonshot_cols, expected_match_detail_shot_cols)
#   details <- foobar_get_match_details(c(3609987, 3609979))
#
#   expect_gt(nrow(details), 0)
#   expect_match_detail_shot_cols(details)
#
#   ## non-domestic match
#   details <- foobar_get_match_details(3846342)
#   expect_gt(nrow(details), 0)
#   expect_match_detail_shot_cols(details)
#
#   ## match with no shots
#   details <- foobar_get_match_details(3361745)
#   expect_equal(nrow(details), 1)
#   expect_match_detail_nonshot_cols(details)
# })
#
# test_that("foobar_get_match_momentum() works", {
#   testthat::skip_on_cran()
#   expected_cols <- c("match_id", "minute", "value", "debug_title", "type")
#
#   momentum <- foobar_get_match_momentum(3901251)
#   expect_gt(nrow(momentum), 0)
#   expect_true(all(expected_cols %in% colnames(momentum)))
#   expect_setequal(unique(momentum$type), c("main", "alternateModels"))
#   ## there are two versions at time of writing, and the alternativeModels format implies that there could be more in the future
#   expect_gte(length(unique(momentum$debug_title)), 2L)
#
#   ## Pre-2022/23 matches do not have momentum
#   expect_error(
#     foobar_get_match_momentum(3610307),
#     regexp = "No momentum"
#   )
# })
