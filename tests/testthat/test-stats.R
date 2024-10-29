# test_that("foobar_get_season_stats() works", {
#   testthat::skip_on_cran()
#
#   expected_stat_cols <- c("country", "league_name", "league_id", "season_name", "season_id", "stat_league_name", "stat_name", "stat", "participant_name", "particiant_id", "team_id", "stat_value", "sub_stat_value", "minutes_played", "matches_played", "stat_value_count", "rank", "participant_country_code", "team_name")
#   expect_stat_cols <- function(df) {
#     expect_true(all(expected_stat_cols %in% colnames(df)))
#   }
#   epl_team_xg_21_a <- foobar_get_season_stats(
#     league_id = 47,
#     season_name = "2020/2021",
#     stat_name = "Expected goals",
#     team_or_player = "team"
#   )
#   expect_gt(nrow(epl_team_xg_21_a), 0)
#   expect_stat_cols(epl_team_xg_21_a)
#
#   get_epl_season_stats <- function(
#     season_name = "2020/2021",
#     team_or_player = "team",
#     stat_name = ifelse(team_or_player == "team", "Expected goals", "Expected goals (xG)")
#   ) {
#     foobar_get_season_stats(
#       country = "ENG",
#       league_name = "Premier League",
#       season_name = season_name,
#       team_or_player = team_or_player,
#       stat_name = stat_name
#     )
#   }
#
#   epl_team_xg_21_b <- get_epl_season_stats()
#
#   expect_identical(epl_team_xg_21_a, epl_team_xg_21_b)
#
#   ## Liga MX has season ids with hyphens
#   liga_mx_team_xg_21 <- foobar_get_season_stats(
#     country = "MEX",
#     league_name = "Liga MX",
#     season = "2021/2022-Clausura",
#     stat_name = "Expected goals",
#     team_or_player = "team"
#   )
#   expect_gt(nrow(liga_mx_team_xg_21), 0)
#   expect_stat_cols(liga_mx_team_xg_21)
#
#   ## foobar has data for 2016/2017 for some leagues and stats, but not all
#   expect_error(
#     get_epl_season_stats(
#       season_name = "2016/2017"
#     )
#   )
#
#   ## foobar doesn't have data this far back for any stat or league
#   expect_error(
#     get_epl_season_stats(
#       season_name = "2010/2011"
#     ),
#     regexp = "not found"
#   )
#
#   epl_player_xg_21 <- get_epl_season_stats(
#     team_or_player = "player"
#   )
#   expect_gt(nrow(epl_player_xg_21), 0)
#   expect_stat_cols(epl_player_xg_21)
#
#   ## similar to team test
#   expect_error(
#     get_epl_season_stats(
#       season = "2016/2017",
#       team_or_player = "player"
#     )
#   )
#
#   ## similar to team test
#   expect_error(
#     get_epl_season_stats(
#       season = "2010/2011",
#       team_or_player = "player"
#     ),
#     regexp = "not found"
#   )
#
#   ## more than one `team_or_player` is not allowed
#   expect_error(
#     get_epl_season_stats(
#       team_or_player = c("team", "player")
#     ),
#     regexp = "one of either"
#   )
#
#   ## invalid `stat_name`
#   expect_error(
#     get_epl_season_stats(
#       stat_name = "foo"
#     )
#   )
#
#   ## invalid `team_or_player`
#   expect_error(
#     get_epl_season_stats(
#       team_or_player = "foo"
#     )
#   )
#
#   ## Does this work for an international tournament?
#   m <- lubridate::month(Sys.Date())
#   if(m >= 1 && m <= 5) {
#     cl_team_xg_21 <- foobar_get_season_stats(
#       league_id = 42,
#       season_name = "2020/2021",
#       stat_name = "Expected goals",
#       team_or_player = "team"
#     )
#     expect_gt(nrow(cl_team_xg_21), 0)
#     expect_stat_cols(cl_team_xg_21)
#   }
#
#   ## see not about MLS from before
#   mls_team_xg_21 <- foobar_get_season_stats(
#     league_id = 130,
#     season_name = "2021",
#     stat_name = "Expected goals",
#     team_or_player = "team"
#   )
#   expect_gt(nrow(mls_team_xg_21), 0)
#   expect_stat_cols(mls_team_xg_21)
#
#   ## multiple leagues
#   epl_ll_team_xg_21 <- foobar_get_season_stats(
#     league_id = c(47, 87),
#     season_name = "2020/2021",
#     stat_name = "Expected goals",
#     team_or_player = "team"
#   )
#
#   expect_gt(nrow(epl_ll_team_xg_21), nrow(epl_team_xg_21_a))
#   expect_stat_cols(epl_ll_team_xg_21)
#
#   ## multiple seasons
#   epl_team_xg_2122 <- get_epl_season_stats(
#     season_name = c("2020/2021", "2021/2022")
#   )
#
#   expect_gt(nrow(epl_team_xg_2122), nrow(epl_team_xg_21_a))
#   expect_stat_cols(epl_team_xg_2122)
#
#   ## more than one stat
#   epl_team_xgs_21 <- get_epl_season_stats(
#     stat_name = c("Expected goals", "xG conceded")
#   )
#   expect_gt(nrow(epl_team_xgs_21), nrow(epl_team_xg_21_a))
#   expect_stat_cols(epl_team_xgs_21)
#
#   ## multiple leagues, seasons, and stats
#   epl_ll_team_xgs_2122 <- foobar_get_season_stats(
#     league_id = c(47, 87),
#     season_name = c("2020/2021", "2021/2022"),
#     stat_name = c("Expected goals", "xG conceded"),
#     team_or_player = "team"
#   )
#
#   expect_gt(nrow(epl_ll_team_xgs_2122), nrow(epl_ll_team_xg_21))
#   expect_gt(nrow(epl_ll_team_xgs_2122), nrow(epl_team_xg_2122))
#   expect_gt(nrow(epl_ll_team_xgs_2122), nrow(epl_team_xgs_21))
#   expect_stat_cols(epl_ll_team_xgs_2122)
# })
