source("app.R")

near <- function(x, y, tolerance = 1e-9) {
  isTRUE(all.equal(x, y, tolerance = tolerance, check.attributes = FALSE))
}

default_model <- build_eigen_model(30, 3, 0.75, 75)
stopifnot(near(t(default_model$P) %*% default_model$P, diag(2)))
stopifnot(near(default_model$A, t(default_model$A)))
stopifnot(near(
  drop(default_model$A %*% default_model$vectors[, 1]),
  default_model$values[1] * default_model$vectors[, 1]
))
stopifnot(near(
  drop(default_model$A %*% default_model$vectors[, 2]),
  default_model$values[2] * default_model$vectors[, 2]
))
stopifnot(near(sum(diag(default_model$A)), sum(default_model$values)))
stopifnot(near(det(default_model$A), prod(default_model$values)))

# Moving only the eigenvalues changes scaling, not either eigen-direction.
changed_values <- build_eigen_model(30, -2, 4, 75)
stopifnot(near(default_model$vectors, changed_values$vectors))
stopifnot(!near(default_model$A, changed_values$A))

# Moving theta rotates the eigen-directions while keeping them perpendicular.
rotated_model <- build_eigen_model(75, 3, 0.75, 20)
stopifnot(abs(sum(rotated_model$vectors[, 1] * rotated_model$vectors[, 2])) < 1e-12)
stopifnot(!near(default_model$vectors, rotated_model$vectors))

# Negative eigenvalues flip an arrow without leaving its eigen-line.
flip_model <- build_eigen_model(30, 2.5, -1, 120)
stopifnot(near(flip_model$transformed_probe, -flip_model$probe))
stopifnot(near(flip_model$line_turn, 0))
stopifnot(near(flip_model$raw_turn, 180))

# Zero eigenvalues flatten one dimension; repeated values make A a scalar I.
flat_model <- build_eigen_model(30, 3, 0, 75)
stopifnot(abs(det(flat_model$A)) < 1e-10)
stopifnot(qr(flat_model$A)$rank == 1)

uniform_model <- build_eigen_model(47, 2, 2, 13)
stopifnot(near(uniform_model$A, 2 * diag(2)))
stopifnot(near(uniform_model$line_turn, 0))

stopifnot(eigenvalue_short_effect(-2) == "flip + stretch")
stopifnot(eigenvalue_short_effect(-0.5) == "flip + shrink")
stopifnot(eigenvalue_short_effect(0) == "flatten")
stopifnot(eigenvalue_short_effect(1) == "same length")

shiny::testServer(server, {
  session$setInputs(
    eig_angle = 30,
    eig_lambda1 = 2.5,
    eig_lambda2 = -1,
    eig_probe_angle = 75
  )
  session$flushReact()

  metrics_html <- paste(as.character(output$eigen_metrics), collapse = "")
  stopifnot(grepl("flip", metrics_html, fixed = TRUE))

  story_html <- paste(as.character(output$eigen_story), collapse = "")
  stopifnot(grepl("Opposite signs", story_html, fixed = TRUE))
  stopifnot(grepl("not the lane angles", story_html, fixed = TRUE))
  stopifnot(grepl("may swap when software", story_html, fixed = TRUE))

  session$setInputs(eig_prediction = "flip", check_eig_prediction = 1)
  session$flushReact()
  feedback_html <- paste(as.character(output$eigen_prediction_feedback), collapse = "")
  stopifnot(grepl("Correct", feedback_html, fixed = TRUE))
  stopifnot(grepl("does not rotate", feedback_html, fixed = TRUE))
})

cat("Eigenvalue geometry, stable directions, teaching states, and prediction feedback passed.\n")
