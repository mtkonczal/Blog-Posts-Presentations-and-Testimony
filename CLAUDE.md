# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a collection of R code for economics blog posts, congressional testimony, and presentations by Mike Konczal. Each project lives in its own directory and is self-contained. There is no build system, CI, or test suite — scripts are run interactively in R/RStudio.

## Structure

- `blogs_2026/` and `blog_posts/` — Blog post analysis projects, organized by date/topic
- `House_Hearing_*`, `IEA_presentation_*`, `New_Paradigm_*` — Testimony and presentation code
- `PCE_Supply_Side_Expansion/` — Research paper code

Each project typically contains:
- Numbered R scripts (`00_setup.R`, `01_*.R`, `02_*.R`) run sequentially
- A `data/` subfolder for intermediate CSV files
- A `graphics/` subfolder for output PNG files
- Optionally `.qmd` (Quarto) or `.Rmd` (R Markdown) documents for rendered reports

## Key R Packages

- **tidyverse** — Used in virtually every script for data manipulation and ggplot2 graphics
- **tidyusmacro** / **govMacroTools** — Author's custom packages for pulling BLS, BEA, and FRED data. `getBLSFiles()`, `getPCEInflation()`, `getFRED()` are common entry points. govMacroTools: https://github.com/mtkonczal/govMacroTools
- **ipumsr** — For loading CPS microdata from IPUMS extracts (XML + .dat files stored locally outside the repo)
- **scales**, **lubridate**, **janitor**, **zoo**, **data.table**, **gt** — Frequently used utilities

## Data Patterns

- BLS/BEA/FRED data is pulled via API using the custom packages above (email `konczal@gmail.com` is passed as the BLS API key parameter)
- CPS microdata is loaded from a local path (`/Users/mtkonczal/Documents/data_folder/`) via ipumsr — these files are not in the repo
- Intermediate data is often saved as CSV to `data/` subfolders, then read back in by downstream scripts or Quarto docs
- Large CSV data files (CPI, PCE) may be committed directly to the repo

## Quarto Documents

Quarto (`.qmd`) files use `code-fold: true` and `execute: echo: true, message: false, warning: false` as standard YAML options. They render to HTML with Bootstrap styling.

## Graphics Style

Scripts use `ggplot2` with `theme_minimal()` as the base theme. Charts are saved via `ggsave()` to `graphics/` directories, typically at 150 dpi. Captions follow the pattern `"Source: [data source], Author's Calculations"`.
