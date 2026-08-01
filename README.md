# Engineering Analysis B Learning Lab

A dark-mode interactive Shiny app built from recurring concepts in six
ENGI-3022 final exams (2020-2025).

The app includes:

- Laplace transform explorer, property lab, and engineering applications
- Differential-equation characteristic-root and phase-portrait labs
- Linear-system lab and a concept-first eigenvalue pattern decoder with a
  transformed-circle graph, stable eigen-directions, presets, and prediction checks
- Exam coverage map with matrix-size frequencies, difficulty proxies,
  efficiency stop rules, a study-time allocator, and a practice generator
- Five 50-mark mock finals with verified answers, displayed algebra, and tutorial matches
- A community feedback board with handles, ratings, comment history, and a live-session count

## Run

```r
install.packages("shiny")
shiny::runApp()
```

The app intentionally uses base R plotting, so `shiny` is its only required
package.

## Comment storage

The community board stores comments in `community-data/comments.rds` when the
host permits file writes. Set `FINALPREP_COMMENT_DIR` to a persistent writable
directory in production. If persistent storage is unavailable, the app falls
back safely and explains that comments may reset after a restart or redeploy.
