# Repository Guidelines

## Project Structure & Module Organization
This folder is a self-contained analysis + writeup bundle built around R scripts and Quarto documents.

- `*.qmd`: Quarto source posts (for example, `affordability_vibes.qmd`, `cex_overview.qmd`).
- `*.html` and `*_files/`: rendered outputs and Quarto assets (generated; do not hand-edit).
- `data/`: input and derived CSVs used by the analysis.
- `graphics/`: exported figures (`.png`) produced by the R scripts.
- `00_*.R`, `01_*.R`, `02_*.R`: ordered, runnable R scripts for data prep and analysis.

## Build, Test, and Development Commands
Run from the repo root (requires R + Quarto):

- `Rscript 00_load_data.R` – pulls CPI/PCE data (uses `tidyusmacro`) and writes raw CSVs.
- `Rscript 00_ces_flatfiles_make_data.R` – prepares CEX flatfile-derived data.
- `Rscript 01_cpi_analysis.R` – builds CPI summaries and saves figures to `graphics/`.
- `Rscript 02_pce_analysis.R` – builds PCE summaries and saves figures to `graphics/`.
- `quarto render affordability_vibes.qmd` – renders the main post to HTML.
- `quarto render cex_overview.qmd` – renders the CEX overview post to HTML.

## Coding Style & Naming Conventions
- Use tidyverse-style R: 2-space indentation, `<-` assignment, and pipe-friendly chaining.
- Prefer `snake_case` for variables and file names.
- Keep script ordering via numeric prefixes (`00_`, `01_`, `02_`).
- Write outputs to `data/` and `graphics/` rather than the repo root.

## Testing Guidelines
- No automated tests are defined for this folder.
- Validate changes by re-running the relevant R scripts and re-rendering the Quarto files.
- Spot-check figures and tables in the generated HTML outputs.

## Commit & Pull Request Guidelines
- Commit messages are short and sentence-case (for example, “Updated title”, “CEX”).
- Keep commits scoped to one logical change and mention data/figure updates when relevant.
- PRs (if used) should include a brief summary, a list of generated artifacts updated, and screenshots or notes for notable chart changes.

## Data & Reproducibility Notes
- Data sources include BLS CPI, BEA PCE, and BLS CEX files; scripts expect those inputs in `data/`.
- Generated HTML and `*_files/` directories should be treated as build outputs.
