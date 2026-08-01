test_store <- file.path(
  tempdir(),
  paste0("finalprep-rocket-game-test-", format(Sys.time(), "%Y%m%d%H%M%S"))
)
dir.create(test_store, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(FINALPREP_COMMENT_DIR = test_store)

source("app.R")
rocket_scores(empty_rocket_scores())

mission_solution <- function(mission, control, damping, lag) {
  evaluate_rocket_game_candidate(mission, control, damping, lag)
}

solutions <- list(
  c(control = .55, damping = .12, lag = .15),
  c(control = .80, damping = .14, lag = .15),
  c(control = 1.05, damping = .18, lag = .15)
)

for (index in seq_along(rocket_game_missions)) {
  solution <- solutions[[index]]
  result <- mission_solution(
    rocket_game_missions[[index]],
    solution[["control"]], solution[["damping"]], solution[["lag"]]
  )
  message(
    sprintf(
      "Mission %d candidate: rightmost=%+.4f, peak=%.3f, checks=%s",
      index, result$rightmost, result$peak_pitch,
      paste(result$checks, collapse = ",")
    )
  )
  stopifnot(all(result$checks))
}

initial_candidate <- evaluate_rocket_game_candidate(
  rocket_game_missions[[1]], 1.30, .025, .82
)
initial_coach <- recommend_rocket_game_move(initial_candidate)
stopifnot(!initial_coach$ready)
stopifnot(nzchar(initial_coach$title))
stopifnot(initial_coach$predicted_rightmost < initial_candidate$rightmost)

ready_candidate <- evaluate_rocket_game_candidate(
  rocket_game_missions[[1]], .55, .12, .15
)
ready_coach <- recommend_rocket_game_move(ready_candidate)
stopifnot(ready_coach$ready)
scoring <- score_rocket_game_mission(ready_candidate, failed_attempts = 2L, elapsed_seconds = 30)
stopifnot(scoring$parts[["base"]] == 750)
stopifnot(scoring$parts[["attempts"]] == -240)
stopifnot(scoring$total == max(300, sum(scoring$parts)))

sample_scores <- data.frame(
  id = as.character(seq_len(14)),
  created_at = sprintf("2026-08-01 %02d:00 UTC", seq_len(14)),
  handle = c("Ada", "ada", paste0("Pilot", seq_len(12))),
  score = c(1800L, 2400L, seq(3000L, 1900L, by = -100L)),
  missions = rep(3L, 14),
  seconds = seq(50, 63),
  stringsAsFactors = FALSE
)
leaders <- top_rocket_scores(sample_scores, 10L)
stopifnot(nrow(leaders) == 10)
stopifnot(!anyDuplicated(tolower(leaders$handle)))
stopifnot(identical(leaders$score, sort(leaders$score, decreasing = TRUE)))
stopifnot(leaders$score[leaders$handle == "ada"] == 2400L)

ui_html <- paste(as.character(ui), collapse = "")
stopifnot(grepl("Play: Rocket Balancer", ui_html, fixed = TRUE))
stopifnot(grepl("rocket_game_handle", ui_html, fixed = TRUE))
stopifnot(grepl("Top 10 players", ui_html, fixed = TRUE))
stopifnot(grepl("Why this trains pattern recognition", ui_html, fixed = TRUE))
stopifnot(grepl("liquid slosh loops back", ui_html, fixed = TRUE))
stopifnot(grepl("Exact scoring", ui_html, fixed = TRUE))
stopifnot(grepl("rocket_game_reset_mission", ui_html, fixed = TRUE))
stopifnot(grepl("github.com/immenseforest/finalprepinteractive/blob/main/app.R", ui_html, fixed = TRUE))

shiny::testServer(server, {
  session$setInputs(
    rocket_game_handle = "A",
    ui_theme = "dark",
    rocket_game_control = 1.30,
    rocket_game_damping = .025,
    rocket_game_lag = .82
  )
  session$setInputs(rocket_game_start = 1)
  session$flushReact()
  stopifnot(nzchar(rocket_game$name_error))
  stopifnot(!isTRUE(rocket_game$active))

  session$setInputs(
    rocket_game_handle = "TestPilot",
    rocket_game_control = 1.30
  )
  session$setInputs(rocket_game_start = 2)
  session$flushReact()

  player_html <- paste(as.character(output$rocket_game_player_status), collapse = "")
  stopifnot(grepl("@TestPilot", player_html, fixed = TRUE))
  stopifnot(grepl("Mission 1 of 3", paste(as.character(output$rocket_game_mission_header), collapse = ""), fixed = TRUE))
  stopifnot(length(output$rocket_game_plot) > 0)
  stopifnot(grepl("Live pattern coach", paste(as.character(output$rocket_game_coach), collapse = ""), fixed = TRUE))

  for (index in seq_along(solutions)) {
    if (index > 1) {
      baseline <- rocket_game_missions[[index]]$initial
      session$setInputs(
        rocket_game_control = baseline[["control"]],
        rocket_game_damping = baseline[["damping"]],
        rocket_game_lag = baseline[["lag"]]
      )
      session$flushReact()
      stopifnot(!isTRUE(rocket_game$transitioning))
    }
    solution <- solutions[[index]]
    session$setInputs(
      rocket_game_control = solution[["control"]],
      rocket_game_damping = solution[["damping"]],
      rocket_game_lag = solution[["lag"]]
    )
    session$flushReact()
    objectives <- paste(as.character(output$rocket_game_objectives), collapse = "")
    stopifnot(length(gregexpr("passed", objectives, fixed = TRUE)[[1]]) >= 5)
    session$setInputs(rocket_game_lock = index)
    session$flushReact()
  }

  stopifnot(isTRUE(rocket_game$completed))
  stopifnot(!isTRUE(rocket_game$active))
  stopifnot(rocket_game$score >= 900L)
  stopifnot(file.exists(rocket_score_store_path))
  leaders_html <- paste(as.character(output$rocket_game_leaderboard), collapse = "")
  stopifnot(grepl("@TestPilot", leaders_html, fixed = TRUE))
})

unlink(test_store, recursive = TRUE, force = TRUE)
cat("Rocket Balancer teaching coach, rules, missions, scoring, persistence, and leaderboard passed.\n")
