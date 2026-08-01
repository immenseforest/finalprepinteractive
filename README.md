# Engineering Analysis B Learning Lab

A dark-mode interactive Shiny app built from recurring concepts in six
ENGI-3022 final exams (2020-2025).

The app includes:

- Laplace transform explorer, property lab, and engineering applications
- Differential-equation characteristic-root and phase-portrait labs
- Linear-system lab and a concept-first eigenvalue pattern decoder with
  transformed-circle and camera-controlled 3D sphere simulations, stable
  eigen-directions, live matrices/equations, presets, and prediction checks
- A frontier-inspired rocket stability lab that connects eigenvalues to
  low-gravity propellant slosh, control lag, orbital refueling, and a live
  five-state disturbance simulation
- Widescreen learning cockpits that align related plots side by side while
  automatically stacking them on tablets and phones
- Exam coverage map with matrix-size frequencies, difficulty proxies,
  efficiency stop rules, a study-time allocator, and a practice generator
- Five 50-mark mock finals with verified answers, displayed algebra, and tutorial matches
- A community feedback board with handles, ratings, comment history, and a live-session count

## Run

```r
install.packages(c("shiny", "plotly"))
shiny::runApp()
```

The app uses base R plotting for most labs and Plotly for the interactive
eigenvalue and rocket-stability cockpits.

## Comment storage

The community board stores comments in `community-data/comments.rds` when the
host permits file writes. Set `FINALPREP_COMMENT_DIR` to a persistent writable
directory in production. If persistent storage is unavailable, the app falls
back safely and explains that comments may reset after a restart or redeploy.
