source("app.R")

stable <- build_rocket_slosh_model(.55, 1.10, .12, .15, .35)
warning <- build_rocket_slosh_model(.85, .85, .02, .45, .65)
unstable <- build_rocket_slosh_model(1.50, .80, .02, 1.20, .70)

stopifnot(identical(dim(stable$A), c(5L, 5L)))
stopifnot(length(stable$eigenvalues) == 5)
stopifnot(max(Re(stable$eigenvalues)) < -.08)
stopifnot(stable$verdict == "Comfortably stable")
stopifnot(max(Re(warning$eigenvalues)) < 0)
stopifnot(max(Re(warning$eigenvalues)) > -.08)
stopifnot(warning$verdict == "Stable, but low margin")
stopifnot(max(Re(unstable$eigenvalues)) > 0)
stopifnot(unstable$verdict == "Unstable")

stable_response <- simulate_rocket_slosh(stable$A)
unstable_response <- simulate_rocket_slosh(unstable$A)
stopifnot(nrow(stable_response) == 1501)
stopifnot(all(is.finite(as.matrix(stable_response))))
stopifnot(max(abs(stable_response$pitch_deg)) <= 1.01)
stopifnot(max(abs(unstable_response$pitch_deg)) > 10)

ui_html <- paste(as.character(ui), collapse = "")
stopifnot(grepl("Frontier: Rocket Stability", ui_html, fixed = TRUE))
stopifnot(grepl("rocket_pole_plot", ui_html, fixed = TRUE))
stopifnot(grepl("rocket_response_plot", ui_html, fixed = TRUE))
stopifnot(grepl("NASA TechPort", ui_html, fixed = TRUE))
stopifnot(grepl("not SpaceX design data", ui_html, fixed = TRUE))

shiny::testServer(server, {
  session$setInputs(
    rocket_control_frequency = 1.50,
    rocket_slosh_frequency = .80,
    rocket_slosh_damping = .02,
    rocket_actuator_lag = 1.20,
    rocket_coupling = .70,
    ui_theme = "dark"
  )
  session$flushReact()

  stopifnot(length(output$rocket_pole_plot) > 0)
  stopifnot(length(output$rocket_response_plot) > 0)

  metrics_html <- paste(as.character(output$rocket_metrics), collapse = "")
  stopifnot(grepl("Unstable", metrics_html, fixed = TRUE))
  stopifnot(grepl("Rightmost Re", metrics_html, fixed = TRUE))

  equations_html <- paste(as.character(output$rocket_equations), collapse = "")
  stopifnot(grepl("Five states produce five eigenvalues", equations_html, fixed = TRUE))
  stopifnot(grepl("live 5", equations_html, fixed = TRUE))

  guidance_html <- paste(as.character(output$rocket_guidance), collapse = "")
  stopifnot(grepl("positive real part", guidance_html, fixed = TRUE))
  stopifnot(grepl("recover stability margin", guidance_html, fixed = TRUE))
})

cat("Rocket slosh eigenvalues, stability presets, time response, live equations, and guidance passed.\n")
