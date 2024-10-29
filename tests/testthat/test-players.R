test_that("foobar_get_match_players() works", {
  testthat::skip_on_cran()

  ## shouldn't test for exact equality since not all stats may appear for a game (if a no player in the game registers a value for a given stat)
  expected_match_player_cols <- c("match_id", "team_id", "team_name", "id", "using_opta_id", "first_name", "last_name", "image_url", "page_url", "shirt", "is_home_team", "time_subbed_on", "time_subbed_off", "usual_position", "position_row", "role", "is_captain", "subbed_out", "g", "rating_num", "rating_bgcolor", "is_top_rating", "is_match_finished", "fantasy_score_num", "fantasy_score_bgcolor", "home_team_id", "home_team_color", "away_team_id", "away_team_color", "stats_fot_mob_rating", "stats_minutes_played", "stats_saves", "stats_goals_conceded", "stats_x_got_faced", "stats_accurate_passes", "stats_accurate_long_balls", "stats_diving_save", "stats_saves_inside_box", "stats_acted_as_sweeper", "stats_punches", "stats_throws", "stats_high_claim", "stats_recoveries", "stats_touches", "stats_goals", "stats_assists", "stats_total_shots", "stats_chances_created", "stats_expected_assists_x_a", "stats_successful_dribbles", "stats_accurate_crosses", "stats_dispossessed", "stats_tackles_won", "stats_clearances", "stats_headed_clearance", "stats_interceptions", "stats_dribbled_past", "stats_ground_duels_won", "stats_aerial_duels_won", "stats_was_fouled", "stats_fouls_committed", "stats_expected_goals_x_g", "stats_shot_accuracy", "stats_blocks", "stats_corners", "stats_offsides", "stats_blocked_shots", "stats_expected_goals_on_target_x_got", "stats_big_chances_missed", "shotmap", "is_starter")
  expect_expected_match_player_cols <- function(df) {
    expect_true(all(expected_match_player_cols %in% colnames(df)))
  }
  players <- foobar_get_match_players(c(3609987, 3609979))
  expect_gt(nrow(players), 0)
  expect_expected_match_player_cols(players)

  # ## check that stats columns are characters, not nested lists
  # expect_true(is.character(players$stats_accurate_crosses))
  #
  # ## non-domestic league
  # players <- foobar_get_match_players(3846347)
  # expect_gt(nrow(players), 0)
  # expect_expected_match_player_cols(players)
  #
  # ## Does it work with a game with some players missing stats?
  # ##   https://www.fotmob.com/match/2999754/matchfacts/vasco-da-gama-vs-atletico-mg
  # players <- foobar_get_match_players(2999754)
  # expect_gt(nrow(players), 0)
})
