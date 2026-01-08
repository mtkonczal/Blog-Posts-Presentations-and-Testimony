# Repository Guidelines

## Project Structure & Module Organization
- `00_ces_flatfiles_make_data.R` pulls CEX flat files via `tidyusmacro`, filters series, and writes the derived dataset.
- `cex_overview.qmd` is the Quarto report that loads `data/cex_data.csv` for analysis and narrative.
- `data/cex_data.csv` is the generated output used by the report (regenerate rather than edit manually).

## Build, Test, and Development Commands
- `Rscript 00_ces_flatfiles_make_data.R` regenerates the derived CEX dataset (writes `data/cex_data.csv`).
- `quarto render cex_overview.qmd` builds the report to HTML.
- `Rscript -e "rmarkdown::render('cex_overview.qmd')"` is an alternative if you prefer running through R.

## Coding Style & Naming Conventions
- R code follows tidyverse conventions (2-space indentation, pipes with `%>%`).
- Use `snake_case` for objects and file names (e.g., `percent_expenditure`).
- Keep data transformations in clear pipelines; prefer explicit `select()` and `mutate()` steps.
- No formatting or linting tools are configured; keep changes minimal and consistent with existing style.

## Testing Guidelines
- There are no automated tests in this repo today.
- If you add tests, place them under a `tests/` directory and prefer `testthat` with `test-*.R` naming.

## Commit & Pull Request Guidelines
- This directory does not appear to be a Git repo (no history to infer conventions).
- Use concise, imperative commit messages (e.g., "Refresh CEX extract", "Update report narrative").
- PRs should describe data sources, list any regenerated artifacts, and attach rendered output if the report changes.

## Data & Configuration Notes
- The data pull uses `tidyusmacro::getBLSFiles`; ensure credentials/environment are set before running.
- Treat `data/cex_data.csv` as derived; update it only via the R script to keep provenance clear.
