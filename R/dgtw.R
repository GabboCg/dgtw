# Value-weighted average (returns NA on zero total weight)
wavg <- function(x, w) {
    
    valid <- !is.na(x) & !is.na(w)
    if (sum(w[valid]) == 0) return(NA_real_)
    sum(x[valid] * w[valid]) / sum(w[valid])
    
}

# Snap a date vector to calendar end of month
end_of_month <- function(d) {
    
    lubridate::ceiling_date(d, "month") - lubridate::days(1)
    
}

# Build DGTW benchmark portfolios and compute stock-level excess returns
# Returns a data frame: permno, date, ret, jyear, formdate, dgtw_port, dgtw_vwret, dgtw_xret
build_dgtw <- function(wrds, start_date = "1970-01-01", end_date = "2017-12-31") {

    # CRSP Block
    # CRSPv2 (CIZ, Feb 2025+): msf_v2 + stksecurityinfohist
    #   old shrcd 10-11  -> sharetype/securitytype/usincflg/issuertype flags
    #   old cfacpr/cfacshr removed; me = abs(mthprc) * shrout / 1000
    #   old exchcd (int) -> primaryexch ("N"=NYSE, "A"=AMEX, "Q"=NASDAQ)
    message("Pulling CRSP data...")
    crsp_raw <- DBI::dbGetQuery(wrds, glue::glue("
        SELECT a.permno, a.permco, a.mthcaldt AS date,
               b.primaryexch AS exchcd, b.siccd,
               a.mthret AS ret, a.shrout, a.mthprc AS prc
        FROM crsp.msf_v2 AS a
        INNER JOIN crsp.stksecurityinfohist AS b
            ON  a.permno = b.permno
            AND b.secinfostartdt <= a.mthcaldt
            AND a.mthcaldt <= b.secinfoenddt
        WHERE a.mthcaldt BETWEEN '{start_date}' AND '{end_date}'
          AND b.sharetype = 'NS'
          AND b.securitytype = 'EQTY'
          AND b.securitysubtype = 'COM'
          AND b.usincflg = 'Y'
          AND b.issuertype IN ('ACOR', 'CORP')
    "))

    crsp_m <- crsp_raw |>
        mutate(
            date   = as.Date(date),
            permco = as.integer(permco),
            permno = as.integer(permno),
            jdate  = end_of_month(date),
            me     = abs(prc) * shrout / 1000   # market cap in $mil
        )

    # Sum market cap across all permnos sharing the same permco on a given date
    crsp_m <- crsp_m |>
        group_by(jdate, permco) |>
        mutate(me_comp = sum(me, na.rm = TRUE)) |>
        ungroup()

    # Compustat Block
    message("Pulling Compustat data...")
    comp_raw <- DBI::dbGetQuery(wrds, "
        SELECT gvkey, datadate, cusip,
               sich, seq, pstkrv, pstkl, pstk, txdb, itcb
        FROM comp.funda
        WHERE indfmt = 'INDL'
          AND datafmt = 'STD'
          AND popsrc = 'D'
          AND consol = 'C'
          AND datadate >= '1970-01-01'
    ")

    comp <- comp_raw |>
        mutate(
            datadate = as.Date(datadate),
            year     = lubridate::year(datadate)
        ) |>
        filter(seq > 0) |>
        mutate(
            # Preferred stock: redemption value > liquidating value > carrying value
            pref = case_when(
                !is.na(pstkrv) ~ pstkrv,
                !is.na(pstkl)  ~ pstkl,
                !is.na(pstk)   ~ pstk,
                TRUE           ~ 0
            ),
            txdb = if_else(is.na(txdb), 0, txdb),
            itcb = if_else(is.na(itcb), 0, itcb),
            # BE = stockholders equity + deferred taxes + ITC - preferred stock
            be   = seq + txdb + itcb - pref
        ) |>
        filter(be >= 0) |>
        select(gvkey, datadate, year, be, sich)

    # CCM Link
    # CRSPv2: ccmxpf_lnkhist replaces ccmxpf_linktable (no usedflag column)
    message("Pulling CCM link table...")
    ccm_raw <- DBI::dbGetQuery(wrds, "
        SELECT gvkey, lpermno AS permno, lpermco AS permco,
               linktype, linkprim, linkdt, linkenddt
        FROM crsp.ccmxpf_lnkhist
        WHERE (linktype = 'LU' OR linktype = 'LC')
          AND linkprim IN ('P', 'C')
    ")

    ccm <- ccm_raw |>
        mutate(
            linkdt    = as.Date(linkdt),
            linkenddt = if_else(is.na(as.Date(linkenddt)), Sys.Date(), as.Date(linkenddt))
        )

    # Merge Compustat with CCM and impose valid date ranges
    ccm2 <- comp |>
        left_join(ccm, by = "gvkey", relationship = "many-to-many") |>
        mutate(
            jdate = end_of_month(datadate),
            year  = lubridate::year(datadate)
        ) |>
        filter(datadate >= linkdt, datadate <= linkenddt) |>
        select(-linktype, -linkdt, -linkenddt)

    # Merge CRSP + Compustat
    message("Merging CRSP and Compustat...")
    comp1 <- ccm2 |>
        inner_join(
            select(crsp_m, permno, permco, date, jdate, siccd, me, me_comp),
            by = c("permco", "permno", "jdate")
        ) |>
        mutate(
            bm = if_else(me_comp > 0, be / me_comp, NA_real_)
        )

    # Remove duplicates and keep max datadate per permno-year
    # (handles firms that change their fiscal year end)
    comp2 <- comp1 |>
        arrange(permno, year, datadate, linkprim, bm) |>
        distinct()

    comp3 <- comp2 |>
        group_by(permno, year) |>
        filter(datadate == max(datadate)) |>
        ungroup()

    # FF48 Industry Classification
    message("Assigning FF48 industries...")
    comp4 <- comp3 |>
        mutate(
            # Use Compustat historical SIC first; fall back to CRSP SIC
            sic = if_else(!is.na(sich) & sich > 0, sich, siccd),
            sic = case_when(
                sic %in% c(3990L, 9995L, 9997L) &
                    !is.na(siccd) & siccd > 0 & sic != siccd ~ siccd,
                TRUE ~ sic
            ),
            sic        = if_else(sic %in% c(3990L, 3999L), 3991L, as.integer(sic)),
            ffi48      = assign_ffi48(sic),
            ffi48_desc = ffi48_labels[as.character(ffi48)]
        ) |>
        filter(!is.na(ffi48_desc), ffi48_desc != "") |>
        select(-sich, -siccd, -datadate) |>
        arrange(ffi48, year)

    # Industry BM Average
    # Long-run cumulative industry BM average up to and including each year
    message("Computing industry BM averages...")
    bm_ind <- comp4 |>
        filter(ffi48 > 0, !is.na(bm), bm >= 0) |>
        group_by(ffi48, year) |>
        summarise(bmind = mean(bm, na.rm = TRUE), .groups = "drop") |>
        arrange(ffi48, year) |>
        group_by(ffi48) |>
        mutate(
            n     = row_number() - 1L,   # 0-indexed cumulative count
            sumbm = cumsum(bmind),
            bmavg = sumbm / (n + 1)
        ) |>
        select(ffi48, year, bmind, bmavg) |>
        ungroup()

    comp5 <- comp4 |>
        left_join(bm_ind, by = c("ffi48", "year")) |>
        mutate(bm_adj = bm - bmavg)

    # Momentum Factor
    # (12,1) momentum: 12-month log-cumulative return, lagged one month
    # Requires at least 7 months of non-missing returns in the 12-month window
    # Missing returns are replaced with 0 before rolling to keep the window intact
    # (preserving non-trading months in the series; see V4 note in notebook)
    message("Computing (12,1) momentum...")
    tmp_crsp <- crsp_m |>
        select(permno, date, ret, me, exchcd) |>
        arrange(permno, date) |>
        mutate(
            ret_fill = if_else(is.na(ret), 0, ret),
            logret   = log(1 + ret_fill)
        )

    tmp_crsp <- tmp_crsp |>
        group_by(permno) |>
        mutate(
            cumlogret = slider::slide_dbl(logret, ~ ifelse(length(.x) >= 7, sum(.x), NA_real_), .before = 11L, .complete = FALSE),
            cumret = exp(cumlogret) - 1,
            mom    = lag(cumret, 1L)   # skip most recent month -> (12,1)
        ) |>
        ungroup()

    # Keep June observations only (annual portfolio formation month)
    sizemom <- tmp_crsp |>
        filter(lubridate::month(date) == 6L) |>
        arrange(date, permno) |>
        distinct() |>
        select(permno, date, mom, me, exchcd, ret_fill) |>
        rename(size = me, ret = ret_fill)

    # NYSE Size Breakpoints
    # Quintile breakpoints based on NYSE stocks only (primaryexch == "N")
    nyse_break <- sizemom |>
        filter(exchcd == "N") |>
        group_by(date) |>
        summarise(
            dec20 = quantile(size, 0.20, na.rm = TRUE),
            dec40 = quantile(size, 0.40, na.rm = TRUE),
            dec60 = quantile(size, 0.60, na.rm = TRUE),
            dec80 = quantile(size, 0.80, na.rm = TRUE),
            .groups = "drop"
        )

    sizemom <- sizemom |>
        left_join(nyse_break, by = "date") |>
        mutate(
            group = case_when(
                size >= 0 & size < dec20 ~ 1L,
                size < dec40             ~ 2L,
                size < dec60             ~ 3L,
                size < dec80             ~ 4L,
                size >= dec80            ~ 5L,
                TRUE                     ~ NA_integer_
            ),
            # year = formation year - 1 to match with prior fiscal-year Compustat data
            year = lubridate::year(date) - 1L
        ) |>
        select(permno, date, year, mom, group, size, ret)

    # Merge Adjusted BM with Size/Momentum
    comp6 <- comp5 |>
        select(gvkey, permno, year, bm_adj) |>
        inner_join(sizemom, by = c("permno", "year")) |>
        drop_na(size, mom, bm_adj, ret)

    # Triple Sort: Size x BM x Momentum
    # 5x5x5 = 125 DGTW benchmark portfolios
    # Portfolios formed each June; held July t through June t+1
    message("Building DGTW portfolios...")
    port1 <- comp6 |>
        arrange(date, group, permno) |>
        distinct() |>
        group_by(date, group) |>
        mutate(bmr = ntile(bm_adj, 5)) |>
        ungroup()

    port2 <- port1 |>
        arrange(date, group, bmr) |>
        group_by(date, group, bmr) |>
        mutate(momr = ntile(mom, 5)) |>
        ungroup()

    # dgtw_port is a 3-character string: size quintile | bm quintile | mom quintile
    # e.g. "143" = smallest size (1), highest BM (4), mid momentum (3)
    port4 <- port2 |>
        mutate(
            dgtw_port = paste0(group, bmr, momr),
            date      = end_of_month(date),
            jyear     = lubridate::year(date)
        ) |>
        rename(formdate = date, sizew = size) |>
        select(permno, formdate, jyear, sizew, dgtw_port) |>
        arrange(permno, formdate)

    # Match Monthly Returns to Formation-Year Portfolios
    # jyear = year(date - 6 months) maps Jul t -> Jun t+1 returns to June t portfolio
    crsp_m1 <- crsp_m |>
        select(permno, date, ret) |>
        mutate(
            date  = end_of_month(date),
            jyear = lubridate::year(date %m-% months(6))
        ) |>
        left_join(port4, by = c("permno", "jyear")) |>
        drop_na(formdate, sizew, dgtw_port) |>
        arrange(date, dgtw_port, permno)

    # Value-Weighted Portfolio Returns & DGTW Excess Returns
    message("Computing VW portfolio returns and DGTW excess returns...")
    dgtw_vwret <- crsp_m1 |>
        group_by(date, dgtw_port) |>
        summarise(dgtw_vwret = wavg(ret, sizew), .groups = "drop")

    dgtw_returns <- crsp_m1 |>
        select(-sizew) |>
        left_join(dgtw_vwret, by = c("dgtw_port", "date")) |>
        mutate(dgtw_xret = ret - dgtw_vwret) |>
        arrange(permno, date) |>
        distinct()

    message("Done.")
    return(dgtw_returns)

}
