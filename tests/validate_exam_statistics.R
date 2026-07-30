source("app.R")

stopifnot(identical(exam_matrix_dimension_counts$year, 2020:2025))
stopifnot(sum(exam_matrix_dimension_counts$total_groups) == 38L)
stopifnot(sum(exam_matrix_dimension_counts$two_by_two) == 8L)
stopifnot(sum(exam_matrix_dimension_counts$three_by_three) == 28L)
stopifnot(sum(exam_matrix_dimension_counts$mixed_or_abstract) == 2L)
stopifnot(sum(exam_matrix_dimension_counts$four_by_four_or_larger) == 0L)
stopifnot(all(
  exam_matrix_dimension_counts$total_groups ==
    exam_matrix_dimension_counts$two_by_two +
      exam_matrix_dimension_counts$three_by_three +
      exam_matrix_dimension_counts$mixed_or_abstract +
      exam_matrix_dimension_counts$four_by_four_or_larger
))

stopifnot(sum(exam_linear_demand_counts$groups) == 21L)
stopifnot(identical(exam_linear_demand_counts$groups, c(10L, 5L, 6L)))
stopifnot(abs(sum(exam_linear_demand_counts$share) - 1) < 1e-12)

stopifnot(abs(sum(recent_study_weights) - 1) < 1e-12)
stopifnot(identical(
  unname(recent_study_weights),
  c(0.51, 0.30, 0.19)
))

linear_frequency <- exam_pattern_frequency[
  exam_pattern_frequency$domain == "Linear algebra",
  ,
  drop = FALSE
]
stopifnot(
  linear_frequency$papers[
    linear_frequency$pattern == "Eigenvalues or eigenvectors"
  ] == 6L
)
stopifnot(
  linear_frequency$papers[
    linear_frequency$pattern == "Dense 4 x 4 or larger computation"
  ] == 0L
)
stopifnot(all(exam_pattern_frequency$papers >= 0L))
stopifnot(all(exam_pattern_frequency$papers <= 6L))

shiny::testServer(server, {
  session$setInputs(study_hours = 10)
  session$flushReact()
  budget_html <- paste(as.character(output$study_budget), collapse = "")
  stopifnot(grepl("5.1 h", budget_html, fixed = TRUE))
  stopifnot(grepl("3.0 h", budget_html, fixed = TRUE))
  stopifnot(grepl("1.9 h", budget_html, fixed = TRUE))

  session$setInputs(efficiency_topic = "Linear algebra")
  session$flushReact()
  scope_html <- paste(as.character(output$efficiency_scope), collapse = "")
  stopifnot(grepl("38 matrix-focused question groups", scope_html, fixed = TRUE))
  stopifnot(grepl("dense 6 x 6 computation", scope_html, fixed = TRUE))

  session$setInputs(frequency_domain = "Differential equations")
  session$flushReact()
  frequency_html <- paste(as.character(output$exam_frequency_table), collapse = "")
  stopifnot(grepl("Standalone nonlinear ODE", frequency_html, fixed = TRUE))
  stopifnot(grepl("3/6", frequency_html, fixed = TRUE))
})

cat("Exam size, demand, frequency, scope, and study-budget checks passed.\n")
