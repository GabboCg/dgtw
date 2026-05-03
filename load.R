#!/usr/bin/env Rscript
# ======================================================== #
#
#              Characteristics-Based Benchmarks
#
#                 Gabriel E. Cabrera-Guzmán
#                The University of Manchester
#
#                       Spring, 2026
#
#                https://gcabrerag.rbind.io
#
# ------------------------------ #
# email: gabriel.cabreraguzman@postgrad.manchester.ac.uk
# ======================================================== #

# Load packages
library(DBI)
library(RPostgres)
library(dplyr)
library(lubridate)
library(slider)
library(tidyr)

# Load auxiliary functions
source("R/ffi48.R")
source("R/dgtw.R")

# WRDS connection
wrds <- dbConnect(
  RPostgres::Postgres(),
  host     = "wrds-pgdata.wharton.upenn.edu",
  port     = 9737,
  dbname   = "wrds",
  user     = Sys.getenv("WRDS_USER"),
  password = Sys.getenv("WRDS_PASSWORD"),
  sslmode  = "require"
)

# Sample period
start_date <- "1970-01-01"
end_date   <- "2025-12-31"

# Create output folder if needed
if (!dir.exists("data")) dir.create("data")

# Build DGTW portfolios
dgtw_returns <- build_dgtw(wrds, start_date = start_date, end_date = end_date)

dbDisconnect(wrds)

# Inspect results
print(head(dgtw_returns))

# Save
write.csv(dgtw_returns, "data/dgtw_returns.csv", row.names = FALSE)
