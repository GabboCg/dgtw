# Characteristics-Based Benchmarks (DGTW)

An R replication of the Daniel, Grinblatt, Titman & Wermers (1997, JF) characteristics-based benchmark methodology. Constructs 125 size × book-to-market × momentum portfolios and computes stock-level benchmark-adjusted (DGTW) excess returns.

[![R](https://img.shields.io/badge/R-%3E%3D4.1-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![WRDS](https://img.shields.io/badge/Data-WRDS-003366)](https://wrds-www.wharton.upenn.edu/)

## Data

Requires access to **WRDS** (Wharton Research Data Services):

| Source | Tables | Content |
|---|---|---|
| CRSP | `crsp.msf_v2`, `crsp.stksecurityinfohist` | Monthly stock returns, prices, shares outstanding |
| Compustat | `comp.funda` | Annual fundamentals (book equity) |
| CCM | `crsp.ccmxpf_lnkhist` | CRSP–Compustat linking table |

> Uses the **CRSPv2/CIZ format** (effective February 2025). Not compatible with the legacy `crsp.msf` + `crsp.msenames` tables.

## Setup

**1. Install R packages**

```r
install.packages(c("DBI", "RPostgres", "dplyr", "lubridate", "slider", "tidyr", "glue"))
```

**2. Set WRDS credentials**

Add to `~/.Renviron`:

```
WRDS_USER=your_username
WRDS_PASSWORD=your_password
```

Then update `load.R` to use:

```r
user     = Sys.getenv("WRDS_USER"),
password = Sys.getenv("WRDS_PASSWORD"),
```

## Usage

Open `dgtw.Rproj` in RStudio and run `load.R`, or from the terminal:

```bash
Rscript load.R
```

Output is saved to `data/dgtw_returns.csv` with the following columns:

| Column | Description |
|---|---|
| `permno` | CRSP stock identifier |
| `date` | Month-end date |
| `ret` | Raw monthly return |
| `jyear` | Formation year (June portfolio year) |
| `formdate` | Portfolio formation date (end of June) |
| `dgtw_port` | 3-digit portfolio code (size \| BM \| momentum quintile) |
| `dgtw_vwret` | Value-weighted benchmark portfolio return |
| `dgtw_xret` | DGTW excess return (`ret - dgtw_vwret`) |

## Methodology

Portfolios are formed each **June** using three sequential sorts:

1. **Size** — 5 quintiles based on NYSE market cap breakpoints
2. **Book-to-market** — 5 quintiles within each size group, using industry-adjusted BM (firm BM minus long-run industry average)
3. **Momentum** — 5 quintiles within each size-BM group, using (12,1) momentum (12-month cumulative log return, skipping the most recent month)

This produces **125 benchmark portfolios** (`dgtw_port` codes `111` through `555`). Each stock is assigned to its June portfolio and held from **July t through June t+1**.

Industry classification uses **Fama-French 48 industries**, with SIC codes sourced from Compustat (`sich`) when available, falling back to CRSP (`siccd`).

## Project Structure

```
.
├── dgtw.Rproj      # RStudio project file
├── load.R          # Entry point: connects to WRDS, runs pipeline, saves output
├── R/
│   ├── ffi48.R     # Fama-French 48 industry classification
│   └── dgtw.R      # build_dgtw() — full portfolio construction pipeline
└── data/
    └── dgtw_returns.csv
```

## Reference

- Daniel, K., Grinblatt, M., Titman, S., & Wermers, R. (1997). Measuring mutual fund performance with characteristic-based benchmarks. *Journal of Finance*, 52(3), 1035–1058. https://doi.org/10.1111/j.1540-6261.1997.tb02724.x
