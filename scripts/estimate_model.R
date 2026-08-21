
# Settings ----

max_lag <- 60


# Load data ----

game_log <- data.table::fread("input/data/pitcher_game_log.csv") |>
  # For the extremely rare case that a pitcher appears in multiple games on the same "day"
  # (because of suspended/resumed games or double-headers?), arbitrarily take one of thoes games.
  dplyr::group_by(pitcher, game_date) |>
  dplyr::slice(1) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    date_game = lubridate::date(game_date),
    date_game_int = as.integer(date_game)
  ) |>
  dplyr::select(
    level = Lvl, player_id = pitcher, game_id = game_pk, year, date_game, date_game_int, pitch_count
  )

injury_list <- pitchinj::etl_injury_list() |>
  dplyr::select(player_id, date_injury = date_listed)

hits <- pitchinj::etl_hits() |>
  dplyr::mutate(date_injury = lubridate::mdy(`Injury Date`)) |>
  dplyr::distinct(player_id = `Player ID`, date_injury)

injury <- injury_list


# Prep data ----

pitcher_year_role <- game_log |>
  dplyr::group_by(player_id, year = lubridate::year(date_game)) |>
  dplyr::summarize(
    # TODO: refine this definition of pitcher role
    role = ifelse(sum(pitch_count) / dplyr::n() > 75, "starter", "reliever"),
    .groups = "drop"
  )

# TODO: Maybe we need to circle back and allow pitchers to be injured on days when they're not
# pitching because we can't find the matching game for a lot of these injuries.
injury_matched <- injury |>
  # TODO: Make sure date ranges line up correctly for game logs and injury data
  dplyr::filter(
    date_injury >= min(game_log$date_game),
    date_injury <= max(game_log$date_game),
  ) |>
  dplyr::inner_join(
    game_log,
    by = dplyr::join_by(player_id, dplyr::closest(date_injury >= date_game))
  ) |>
  dplyr::filter(date_injury - date_game <= 5) |>
  # In the rare case that a pitcher has two injury records matched to the same game, keep the first
  dplyr::group_by(player_id, game_id) |>
  dplyr::summarize(date_injury = min(date_injury), .groups = "drop")

data_wce <- game_log |>
  dplyr::left_join(pitcher_year_role, by = c("player_id", "year")) |>
  dplyr::left_join(injury_matched, by = c("player_id", "game_id")) |>
  dplyr::mutate(stint = cumsum(!is.na(dplyr::lag(date_injury, 1))), .by = c(player_id, year)) |>
  dplyr::mutate(stint_start = min(date_game_int), .by = c(player_id, year, stint)) |>
  dplyr::mutate(
    player_id = as.factor(player_id),
    is_injury = !is.na(date_injury),
    year_factor = as.factor(year),
    stint_day = date_game_int - stint_start
  )


# Set up the workload matrices ----

date_game_int_split <- split(game_log$date_game_int, game_log$player_id)
pitch_count_split <- split(game_log$pitch_count, game_log$player_id)
pitcher_index_split <- split(1:nrow(game_log), game_log$player_id)

workload_matrix <- matrix(0, nrow = nrow(game_log), ncol = max_lag)

for (id in names(date_game_int_split)) {
  date_game_int_id <- date_game_int_split[[id]]
  pitch_count_id <- pitch_count_split[[id]]
  pitcher_index_id <- pitcher_index_split[[id]]

  lag_date_game_int <- outer(date_game_int_id, 1:max_lag, "-")
  pitch_count_lookup <- match(as.vector(lag_date_game_int), date_game_int_id)

  workload_vector <- dplyr::coalesce(pitch_count_id[pitch_count_lookup], 0)

  workload_matrix[pitcher_index_id, ] <- workload_vector |>
    matrix(nrow = length(date_game_int_id))
}

lag_matrix <- matrix(1:max_lag, nrow = nrow(game_log), ncol = max_lag, byrow = TRUE)

lag_matrix <- lag_matrix[data_wce$level == "MLB", ]
workload_matrix <- workload_matrix[data_wce$level == "MLB", ]
data_wce <- data_wce[data_wce$level == "MLB", ]


# Estimate model ----

.time <- Sys.time()
fit_wce <- mgcv::bam(
  is_injury ~
    role +
#    s(player_id, bs = "re") +
    s(
      lag_matrix,
      by = workload_matrix,
      k = 15,
      bs = "fdl",
      m = c(2, 2),
      xt = list(constrain = FALSE, ridge = FALSE)
    ) +
    s(stint_day, k = 10, bs = "ps", m = c(2, 2)),
  data = data_wce,
  family = binomial(link = "cloglog"),
  method = "fREML",
  discrete = TRUE
)
print(Sys.time() - .time)

saveRDS(fit_wce, file = "output/fit_wce.rds")
