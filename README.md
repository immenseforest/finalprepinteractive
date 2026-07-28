# Engineering Analysis B Learning Lab

A dark-mode interactive Shiny app built from recurring concepts in six
ENGI-3022 final exams (2020-2025).

The app includes:

- Laplace transform explorer, property lab, and engineering applications
- Differential-equation characteristic-root and phase-portrait labs
- Linear-system and eigenvalue labs
- Exam coverage map, mastery checklist, and practice generator

## Run

```r
install.packages("shiny")
shiny::runApp()
```

The app intentionally uses base R plotting, so `shiny` is its only required
package.
