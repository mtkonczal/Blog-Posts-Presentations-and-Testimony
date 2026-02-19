import pandas as pd
import numpy as np
import requests
from pathlib import Path
from datetime import datetime

DATA_DIR = Path("/Users/mtkonczal/Documents/good4u_inflation/data")
DATA_DIR.mkdir(parents=True, exist_ok=True)

FRED_SERIES = {
    "CPIAUCSL": "cpi",
    "PCEPILFE": "core_pce",
    "UNRATE": "unrate",
    "UNEMPLOY": "unemp",
    "JTSJOL": "job_open",
    "NROU": "nairu",
}

SPF_URL = (
    "https://www.philadelphiafed.org/-/media/FRBP/Assets/Surveys-And-Data/"
    "survey-of-professional-forecasters/data-files/files/Mean_CPI10_Level.xlsx"
    "?hash=25A051025B7E9CC911D135B577C44502&sc_lang=en"
)

GSCPI_URL = "https://www.newyorkfed.org/medialibrary/research/interactives/gscpi/downloads/gscpi_data.xlsx"

KWORB_URL = "https://kworb.net/youtube/video/gNi_6U5Pm_o.html"

OUTPUT_CSV = DATA_DIR / "phillips_data.csv"
GSCPI_CSV = DATA_DIR / "gscpi_monthly.csv"
GOOD4U_CSV = DATA_DIR / "olivia_good4u_views.csv"


def fred_csv(series_id: str, out_name: str) -> pd.DataFrame:
    url = f"https://fred.stlouisfed.org/graph/fredgraph.csv?id={series_id}"
    df = pd.read_csv(url)
    date_col = [c for c in df.columns if "date" in c.lower()][0]
    value_col = [c for c in df.columns if c != date_col][0]
    df = df.rename(columns={date_col: "date", value_col: out_name})
    df["date"] = pd.to_datetime(df["date"])
    return df[["date", out_name]]


def download_spf_cpi10() -> pd.DataFrame:
    df = pd.read_excel(SPF_URL)
    df.columns = [c.lower() for c in df.columns]
    if not {"year", "quarter"}.issubset(df.columns):
        raise ValueError("SPF file missing year/quarter columns.")
    cpi_col = "cpi10" if "cpi10" in df.columns else df.columns[-1]
    df["exp_pi10"] = pd.to_numeric(df[cpi_col], errors="coerce")
    df = df.dropna(subset=["exp_pi10"]).copy()
    df["qtr"] = pd.PeriodIndex(year=df["year"].astype(int), quarter=df["quarter"].astype(int), freq="Q")
    return df[["qtr", "exp_pi10"]]


def download_gscpi_monthly() -> pd.DataFrame:
    tmp = DATA_DIR / "_gscpi_tmp.xlsx"
    resp = requests.get(GSCPI_URL, timeout=30)
    resp.raise_for_status()
    tmp.write_bytes(resp.content)

    df = pd.read_excel(tmp, sheet_name="GSCPI Monthly Data")
    df = df.loc[df["Date"].notna(), ["Date", "GSCPI"]].copy()
    df["date"] = pd.to_datetime(df["Date"]).dt.to_period("M").dt.to_timestamp()
    df = df[["date", "GSCPI"]].rename(columns={"GSCPI": "gscpi"})

    df.to_csv(GSCPI_CSV, index=False)
    tmp.unlink(missing_ok=True)
    return df


def parse_kworb_good4u() -> pd.DataFrame:
    tables = pd.read_html(KWORB_URL)

    def pick_table(tables_list):
        for t in tables_list:
            cols = [str(c).lower() for c in t.columns]
            has_month = any("month" in c for c in cols)
            has_views = any("view" in c for c in cols)
            if has_month and has_views:
                return t
        return None

    tbl = pick_table(tables)
    if tbl is None:
        raise ValueError("Could not find monthly views table on kworb page.")

    cols = {c.lower(): c for c in tbl.columns}
    month_col = cols[[c for c in cols if "month" in c][0]]
    views_col = cols[[c for c in cols if "view" in c][0]]

    out = tbl[[month_col, views_col]].copy()
    out.columns = ["month", "views"]

    out["views"] = (
        out["views"]
        .astype(str)
        .str.replace(r"[^0-9.]", "", regex=True)
        .replace("", np.nan)
        .astype(float)
    )

    out["date"] = pd.to_datetime(out["month"], errors="coerce")
    if out["date"].isna().all():
        out["date"] = pd.to_datetime(out["month"] + "-01", errors="coerce")

    out = out.dropna(subset=["date"]).copy()
    out["date"] = out["date"].dt.to_period("M").dt.to_timestamp()

    out = out[["date", "views"]]
    out.to_csv(GOOD4U_CSV, index=False)
    return out


def quarterly_average(df: pd.DataFrame, value_col: str) -> pd.DataFrame:
    q = df.set_index("date")[value_col].resample("Q").mean().to_frame("value")
    q["qtr"] = q.index.to_period("Q")
    return q.reset_index(drop=True)


def quarterly_sum(df: pd.DataFrame, value_col: str) -> pd.DataFrame:
    q = df.set_index("date")[value_col].resample("Q").sum().to_frame("value")
    q["qtr"] = q.index.to_period("Q")
    return q.reset_index(drop=True)


def main():
    # FRED monthly series
    cpi_m = fred_csv("CPIAUCSL", "cpi")
    core_pce_m = fred_csv("PCEPILFE", "core_pce")
    unrate_m = fred_csv("UNRATE", "unrate")
    unemp_m = fred_csv("UNEMPLOY", "unemp")
    job_open_m = fred_csv("JTSJOL", "job_open")
    nairu_m = fred_csv("NROU", "nairu")

    # SPF expected inflation
    spf_q = download_spf_cpi10()

    # GSCPI monthly data
    gscpi_m = download_gscpi_monthly()

    # Good 4 U monthly views
    good4u_m = parse_kworb_good4u()

    # Fill pre-release months with zero views
    date_min = min(cpi_m["date"].min(), gscpi_m["date"].min())
    date_max = max(cpi_m["date"].max(), gscpi_m["date"].max())
    all_months = pd.date_range(date_min, date_max, freq="MS")
    good4u_m = (
        pd.DataFrame({"date": all_months})
        .merge(good4u_m, on="date", how="left")
        .fillna({"views": 0})
    )

    # Quarterly CPI inflation (annualized log diff)
    cpi_q = quarterly_average(cpi_m, "cpi")
    cpi_q = cpi_q.sort_values("qtr").reset_index(drop=True)
    cpi_q["pi"] = 400 * (np.log(cpi_q["value"]) - np.log(cpi_q["value"].shift(1)))
    cpi_q = cpi_q.rename(columns={"value": "cpi"})

    # Quarterly core PCE inflation (annualized log diff)
    core_pce_q = quarterly_average(core_pce_m, "core_pce")
    core_pce_q = core_pce_q.sort_values("qtr").reset_index(drop=True)
    core_pce_q["core_pce_pi"] = 400 * (np.log(core_pce_q["value"]) - np.log(core_pce_q["value"].shift(1)))
    core_pce_q = core_pce_q.rename(columns={"value": "core_pce"})

    # Other quarterly series
    unrate_q = quarterly_average(unrate_m, "unrate").rename(columns={"value": "unrate"})
    unemp_q = quarterly_average(unemp_m, "unemp").rename(columns={"value": "unemp"})
    job_open_q = quarterly_average(job_open_m, "job_open").rename(columns={"value": "job_open"})

    nairu_m = nairu_m.loc[nairu_m["date"] <= pd.Timestamp("2025-12-31")].copy()
    nairu_q = quarterly_average(nairu_m, "nairu").rename(columns={"value": "nairu"})

    # v/u from monthly series
    v_u_m = job_open_m.merge(unemp_m, on="date", how="inner")
    v_u_m["v_u"] = v_u_m["job_open"] / v_u_m["unemp"]
    v_u_q = quarterly_average(v_u_m, "v_u").rename(columns={"value": "v_u"})

    gscpi_q = quarterly_average(gscpi_m, "gscpi").rename(columns={"value": "gscpi"})
    good4u_q = quarterly_sum(good4u_m, "views").rename(columns={"value": "good4u_views"})

    # Merge to quarterly dataset
    df = cpi_q[["qtr", "cpi", "pi"]]
    df = df.merge(core_pce_q[["qtr", "core_pce", "core_pce_pi"]], on="qtr", how="left")
    df = df.merge(unrate_q, on="qtr", how="left")
    df = df.merge(unemp_q, on="qtr", how="left")
    df = df.merge(job_open_q, on="qtr", how="left")
    df = df.merge(nairu_q, on="qtr", how="left")
    df = df.merge(v_u_q, on="qtr", how="left")
    df = df.merge(gscpi_q, on="qtr", how="left")
    df = df.merge(good4u_q, on="qtr", how="left")
    df = df.merge(spf_q, on="qtr", how="left")

    df = df.sort_values("qtr").reset_index(drop=True)
    df["pi_l1"] = df["pi"].shift(1)
    df["pi_l2"] = df["pi"].shift(2)
    df["core_pce_pi_l1"] = df["core_pce_pi"].shift(1)
    df["core_pce_pi_l2"] = df["core_pce_pi"].shift(2)
    df["unrate_gap"] = df["unrate"] - df["nairu"]

    df["qtr_date"] = df["qtr"].dt.to_timestamp()
    df["y2021_2022"] = df["qtr"].dt.year.isin([2021, 2022]).astype(int)
    max_views = df["good4u_views"].max()
    if pd.notna(max_views) and max_views > 0:
        df["good4u_views_norm"] = df["good4u_views"] / max_views
    else:
        df["good4u_views_norm"] = np.nan
    df["good4u_views_log"] = np.log1p(df["good4u_views"])

    out_cols = [
        "qtr_date",
        "cpi",
        "pi",
        "pi_l1",
        "pi_l2",
        "core_pce",
        "core_pce_pi",
        "core_pce_pi_l1",
        "core_pce_pi_l2",
        "exp_pi10",
        "unrate",
        "nairu",
        "unrate_gap",
        "unemp",
        "job_open",
        "v_u",
        "gscpi",
        "good4u_views",
        "good4u_views_norm",
        "good4u_views_log",
        "y2021_2022",
    ]

    df[out_cols].to_csv(OUTPUT_CSV, index=False)
    print(f"Wrote {OUTPUT_CSV}")


if __name__ == "__main__":
    main()
