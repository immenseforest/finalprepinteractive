test_store <- file.path(
  tempdir(),
  paste0("finalprep-routes-test-", format(Sys.time(), "%Y%m%d%H%M%S"))
)
dir.create(test_store, recursive = TRUE, showWarnings = FALSE)
Sys.setenv(FINALPREP_COMMENT_DIR = test_store)

source("app.R")

stopifnot(route_scalar("  ?page=exam-practice  ") == "?page=exam-practice")

home <- resolve_app_route(list())
stopifnot(home$page == "home")
stopifnot(home$main_value == "app_about")
stopifnot(home$query == "?page=home")

exam <- resolve_app_route(list(page = "exam-practice"))
stopifnot(exam$main_value == "exam_coach")
stopifnot(exam$nav_id == "exam_navigation")
stopifnot(exam$section_value == "exam_roadmap")
stopifnot(exam$query == "?page=exam-practice&section=roadmap-practice")

mock_exam <- resolve_app_route(list(page = "exam-practice", section = "mock-exam"))
stopifnot(mock_exam$section_value == "mock_exam")
stopifnot(mock_exam$query == "?page=exam-practice&section=mock-exam")

rocket_game <- resolve_app_route(list(page = "linear-algebra", section = "rocket-balancer"))
stopifnot(rocket_game$main_value == "linear_algebra")
stopifnot(rocket_game$section_value == "rocket_balancer")

chemical <- resolve_app_route(list(
  page = "laplace", section = "engineering", example = "chemical-mixing"
))
stopifnot(chemical$section_value == "engineering")
stopifnot(chemical$example_value == "chemical")
stopifnot(chemical$query == "?page=laplace&section=engineering&example=chemical-mixing")

invalid <- resolve_app_route(list(page = "missing", section = "also-missing"))
stopifnot(invalid$page == "home")
stopifnot(invalid$query == "?page=home")

from_tabs <- app_route_from_tab_values(
  "exam_coach", list(exam_navigation = "mock_exam")
)
stopifnot(from_tabs$query == "?page=exam-practice&section=mock-exam")

from_formula <- app_route_from_tab_values(
  "reference", list(reference_navigation = "reference_method_selector")
)
stopifnot(from_formula$query == "?page=formula-library&section=method-selector")

from_engineering <- app_route_from_tab_values(
  "laplace",
  list(laplace_navigation = "engineering", engineering_navigation = "civil")
)
stopifnot(from_engineering$query == "?page=laplace&section=engineering&example=civil-vibration")

ui_html <- paste(as.character(ui), collapse = "")
stable_values <- c(
  "differential_about", "differential_linear_odes", "differential_systems",
  "linear_algebra_about", "linear_algebra_eigenvalues", "rocket_stability",
  "rocket_balancer", "reference_laplace", "reference_method_selector"
)
stopifnot(all(vapply(stable_values, grepl, logical(1), x = ui_html, fixed = TRUE)))

unlink(test_store, recursive = TRUE, force = TRUE)
cat("Friendly main-page, subpage, engineering-example, and fallback routes passed.\n")
