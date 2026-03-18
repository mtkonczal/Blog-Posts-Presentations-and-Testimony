# getFRED() HTTP/2 Bug

## Problem

`tidyusmacro::getFRED()` fails when called from `Rscript` or `quarto render`, but works fine in RStudio interactive sessions.

```
Error in getFRED(...):
! No data downloaded successfully.
Warning: Failed to open 'https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE':
  HTTP/2 stream 1 was not closed cleanly: INTERNAL_ERROR (err 2)
```

## Root Cause

`getFRED()` downloads FRED CSVs via `readr::read_csv(url)`, which uses R's internal `url()` connection. That connection relies on R's **built-in libcurl** (v8.7.1 on this machine), which has an HTTP/2 framing bug when talking to `fred.stlouisfed.org`.

RStudio's R session uses a different HTTP code path (its own bundled libcurl or connection handling), so the same `getFRED()` call succeeds there.

Key details from this machine (as of March 2026):
- R's built-in libcurl: **8.7.1** (the broken one, used by `readr::read_csv(url)`)
- R curl package libcurl: **8.14.1** (also broken for this server -- same LibreSSL/HTTP2 issue)
- SSL: LibreSSL/3.3.6 (SecureTransport)
- System curl (command line): **works fine** (different build, likely with HTTP/2 fixes or OpenSSL)

The R `curl` package (v8.14.1) also fails, so this isn't just about the older built-in version. It appears to be a LibreSSL + HTTP/2 interaction specific to the FRED server.

## Current Workaround

In the QMD, we use `download.file(method = "curl")` which shells out to the system `curl` binary (not R's libcurl):

```r
dl_fred <- function(id) {
  tmp <- tempfile(fileext = ".csv")
  download.file(
    paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", id),
    tmp, method = "curl", quiet = TRUE
  )
  read_csv(tmp, show_col_types = FALSE) |>
    rename(date = 1, value = 2) |>
    mutate(date = as.Date(date))
}
```

## Possible Fixes for getFRED()

1. **Use `download.file(method = "curl")` inside `getFRED()`** instead of `readr::read_csv(url)`. Download to a temp file first, then read it. This is the simplest fix and works everywhere.

2. **Force HTTP/1.1** in the download. If using the `curl` package, set `http_version = 2L` (which is the libcurl constant for `CURL_HTTP_VERSION_1_1`) on the handle. This didn't work with the current curl package build on this machine but may work on others.

3. **Use the `httr2` package** with `req_options(http_version = 2)` to force HTTP/1.1. Untested.

4. **Use `fredr` package** instead of scraping the CSV endpoint. This uses the official FRED API (JSON, not CSV) and avoids the HTTP/2 issue entirely. Requires a FRED API key set as `FRED_API_KEY` in `.Renviron`.

5. **Update R and/or libcurl**. The HTTP/2 framing bug may be fixed in newer builds. Check if reinstalling R from CRAN or updating via Homebrew resolves it.

## How to Test

```r
# This should fail from Rscript, succeed in RStudio:
library(tidyusmacro)
df <- getFRED(unrate = "unrate", fed_funds = "FEDFUNDS", keep_all = FALSE)

# This should always work (uses system curl):
tmp <- tempfile()
download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?id=UNRATE",
              tmp, method = "curl", quiet = TRUE)
readr::read_csv(tmp)
```

## Recommendation

Option 1 (temp file via system curl) is the lowest-risk fix for the `getFRED()` function in `tidyusmacro`/`govMacroTools`. It preserves the existing CSV-scraping approach and just changes the transport layer. The diff would be small: replace `readr::read_csv(url, ...)` with a two-step download-then-read inside the `get_one()` internal function.
