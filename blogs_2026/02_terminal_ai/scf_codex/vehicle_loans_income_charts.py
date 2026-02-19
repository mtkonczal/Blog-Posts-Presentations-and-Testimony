#!/usr/bin/env python3
"""
Create SCF 2022 charts for:
1) Distribution of vehicle installment loan balances by income quintile
2) Average number of vehicles by income quintile
"""

from pathlib import Path

import matplotlib
import numpy as np
import pandas as pd

matplotlib.use("Agg")
import matplotlib.pyplot as plt

DATA_PATH = Path("SCFP2022.csv")
OUTPUT_DIR = Path("outputs")


def weighted_mean(x: np.ndarray, w: np.ndarray) -> float:
    m = np.isfinite(x) & np.isfinite(w)
    x = x[m]
    w = w[m]
    if w.size == 0 or np.sum(w) == 0:
        return np.nan
    return float(np.sum(x * w) / np.sum(w))


def weighted_quantile(x: np.ndarray, w: np.ndarray, q: float) -> float:
    m = np.isfinite(x) & np.isfinite(w)
    x = x[m]
    w = w[m]
    if x.size == 0 or np.sum(w) == 0:
        return np.nan
    idx = np.argsort(x)
    x = x[idx]
    w = w[idx]
    cum = np.cumsum(w)
    return float(np.interp(q * cum[-1], cum, x))


def assign_income_quintile(income: np.ndarray, w: np.ndarray) -> np.ndarray:
    cuts = [weighted_quantile(income, w, q) for q in [0.2, 0.4, 0.6, 0.8]]
    q = np.full(income.shape, np.nan, dtype=float)
    finite = np.isfinite(income)
    q[finite & (income <= cuts[0])] = 1
    q[finite & (income > cuts[0]) & (income <= cuts[1])] = 2
    q[finite & (income > cuts[1]) & (income <= cuts[2])] = 3
    q[finite & (income > cuts[2]) & (income <= cuts[3])] = 4
    q[finite & (income > cuts[3])] = 5
    return q


def main() -> None:
    df = pd.read_csv(
        DATA_PATH,
        usecols=["WGT", "INCOME", "VEH_INST", "HVEH_INST", "NVEHIC"],
    )

    w = df["WGT"].astype(float).to_numpy()
    income = df["INCOME"].astype(float).to_numpy()
    veh_inst = df["VEH_INST"].astype(float).to_numpy()
    has_veh_inst = df["HVEH_INST"].astype(float).to_numpy()
    nveh = df["NVEHIC"].astype(float).to_numpy()

    inc_q = assign_income_quintile(income, w)
    labels = ["Q1 (lowest)", "Q2", "Q3", "Q4", "Q5 (highest)"]

    rows = []
    total_veh_inst_weighted = np.sum(veh_inst * w)
    for quint in range(1, 6):
        m = inc_q == quint
        loan_share = (
            np.sum(veh_inst[m] * w[m]) / total_veh_inst_weighted
            if total_veh_inst_weighted > 0
            else np.nan
        )
        rows.append(
            {
                "Income quintile": labels[quint - 1],
                "Loan balance share": loan_share,
                "Has vehicle installment loan": weighted_mean(has_veh_inst[m], w[m]),
                "Average vehicles per household": weighted_mean(nveh[m], w[m]),
            }
        )

    out = pd.DataFrame(rows)
    OUTPUT_DIR.mkdir(parents=True, exist_ok=True)
    out.to_csv(OUTPUT_DIR / "vehicle_loans_income_summary.csv", index=False)

    # Chart 1: Distribution of vehicle installment balances by income quintile
    fig, ax = plt.subplots(figsize=(9, 5))
    bars = ax.bar(out["Income quintile"], out["Loan balance share"] * 100, color="#1f77b4")
    ax.set_ylabel("Share of all vehicle loan balances (%)")
    ax.set_title("SCF 2022: Vehicle Loan Balances by Income Quintile")
    ax.set_ylim(0, max(5, float((out["Loan balance share"] * 100).max() * 1.2)))
    ax.grid(axis="y", alpha=0.25)
    for bar, value in zip(bars, out["Loan balance share"] * 100):
        ax.text(
            bar.get_x() + bar.get_width() / 2,
            bar.get_height() + 0.4,
            f"{value:.1f}%",
            ha="center",
            va="bottom",
            fontsize=9,
        )
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "vehicle_loan_distribution_by_income_quintile.png", dpi=200)
    plt.close(fig)

    # Chart 2: Number of vehicles by income quintile
    fig, ax = plt.subplots(figsize=(9, 5))
    bars = ax.bar(
        out["Income quintile"],
        out["Average vehicles per household"],
        color="#2ca02c",
    )
    ax.set_ylabel("Average vehicles per household")
    ax.set_title("SCF 2022: Number of Vehicles by Income Quintile")
    ax.set_ylim(0, max(0.5, float(out["Average vehicles per household"].max() * 1.2)))
    ax.grid(axis="y", alpha=0.25)
    for bar, value in zip(bars, out["Average vehicles per household"]):
        ax.text(
            bar.get_x() + bar.get_width() / 2,
            bar.get_height() + 0.03,
            f"{value:.2f}",
            ha="center",
            va="bottom",
            fontsize=9,
        )
    fig.tight_layout()
    fig.savefig(OUTPUT_DIR / "vehicles_per_household_by_income_quintile.png", dpi=200)
    plt.close(fig)


if __name__ == "__main__":
    main()
