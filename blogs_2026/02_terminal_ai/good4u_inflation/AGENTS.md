# Project Summary (good4u_inflation)

This file summarizes the analysis pipeline and report setup for Phillips Curve regressions with alternative augmentations, based on the work done in this repository.

## What the analysis does
- Builds quarterly Phillips Curve regressions using **core PCE inflation** as the dependent variable.
- Uses **expected inflation** (SPF 10-year CPI expectations), two lags of realized inflation, and **unemployment gap** (unemployment rate minus NAIRU) as the baseline specification.
- Adds augmentation variables one at a time and all together:
  - Global Supply Chain Pressure Index (GSCPI)
  - v/u (job openings over unemployed)
  - YouTube views for Olivia Rodrigo “good 4 u” (log1p)
  - 2021–2022 dummy
- Produces multiple regression tables (custom with stars + stargazer + gt + texreg + huxtable + kableExtra).

## Key files
- Data build script: `/Users/mtkonczal/Documents/good4u_inflation/scripts/build_phillips_data.py`
- Output dataset: `/Users/mtkonczal/Documents/good4u_inflation/data/phillips_data.csv`
- Report: `/Users/mtkonczal/Documents/good4u_inflation/phillips_curves.qmd`
- Rendered output: `/Users/mtkonczal/Documents/good4u_inflation/phillips_curves.html`

## Data sources and transformations
All data is downloaded by the build script and assembled into a single quarterly CSV.

### FRED series
- CPI (CPIAUCSL)
- Core PCE (PCEPILFE)
- Unemployment rate (UNRATE)
- Unemployed level (UNEMPLOY)
- Job openings (JTSJOL)
- NAIRU (NROU)

### Other sources
- SPF 10-year CPI expectations (Philadelphia Fed, mean CPI10 Excel file)
- GSCPI monthly data (NY Fed monthly data file)
- Monthly YouTube views for “good 4 u” (Kworb table)

### Transformations
- Quarterly inflation = 400 * log difference of quarterly average price index.
- Lags: `core_pce_pi_l1`, `core_pce_pi_l2`.
- Unemployment gap: `unrate_gap = unrate - nairu`.
- NAIRU is clipped at 2025-12-31 before quarterly averaging.
- YouTube series is converted to quarterly **sum** and used as `log1p(views)`.

## How to run
1) Build data:
```bash
python3 /Users/mtkonczal/Documents/good4u_inflation/scripts/build_phillips_data.py
```

2) Render report:
```bash
quarto render /Users/mtkonczal/Documents/good4u_inflation/phillips_curves.qmd
```

## Current model setup (in QMD)
- Baseline: `core_pce_pi ~ exp_pi10 + core_pce_pi_l1 + core_pce_pi_l2 + unrate_gap`
- Augmentations: add `gscpi`, `v_u`, `good4u_views_log`, `y2021_2022`
- “All four” includes all augmentation variables
- Sample starts in **2000**

## Table outputs in report
- Custom regression table (stars + SEs, NAs blanked)
- Stargazer (HTML, full-width wrapper)
- gt (modelsummary output)
- texreg (HTML)
- huxtable
- kableExtra (scrollable HTML)

## Notes
- NAIRU is forward-projected on FRED; the script clips it to end of 2025.
- YouTube views are sourced from Kworb and not an official Google API time series.
- The report does not include regression coefficient plots or model performance charts (removed by request).
