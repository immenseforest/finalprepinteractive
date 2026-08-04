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
- A three-mission Rocket Balancer mini game with a live one-tick pattern coach,
  ASCII concept diagrams, fully disclosed rules/scoring, named player runs,
  mastery scoring, and a persistent top-10 leaderboard
- Widescreen learning cockpits that align related plots side by side while
  automatically stacking them on tablets and phones
- Friendly shareable URLs for every main page and important subpage, with
  browser Back/Forward restoration
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

## Shareable page links

Append a friendly query route to the deployed app URL. Examples:

- `?page=exam-practice&section=mock-exam`
- `?page=linear-algebra&section=eigenvalues`
- `?page=linear-algebra&section=rocket-balancer`
- `?page=laplace&section=engineering&example=chemical-mixing`
- `?page=formula-library&section=method-selector`

The address bar updates as the user changes pages, and browser Back/Forward
restores the matching main page, subpage, and engineering example.

## Community and game storage

The community board stores comments in `community-data/comments.rds` when the
host permits file writes. Rocket Balancer stores its scores beside it in
`community-data/rocket_scores.rds`. Set `FINALPREP_COMMENT_DIR` to a persistent
writable directory in production. If persistent storage is unavailable, the
app falls back safely and explains that community data may reset after a
restart or redeploy.
