# =========================================================
# COINr Builder — framework metadata & pipeline helpers
# =========================================================

level_names_for <- function(maxlev) {
  switch(as.character(maxlev),
         "2" = c("Indicator", "Index"),
         "3" = c("Indicator", "Pillar", "Index"),
         "4" = c("Indicator", "Pillar", "Sub-index", "Index"),
         paste("Level", seq_len(maxlev)))
}

# Everything the UI needs to know about the framework, derived from iMeta/iData.
fw_info <- function(iData, iMeta) {
  ind    <- iMeta[iMeta$Type == "Indicator", , drop = FALSE]
  aggs   <- iMeta[iMeta$Type == "Aggregate", , drop = FALSE]
  denoms <- iMeta[iMeta$Type == "Denominator", , drop = FALSE]
  groups <- iMeta[iMeta$Type == "Group", , drop = FALSE]
  maxlev <- suppressWarnings(max(iMeta$Level, na.rm = TRUE))
  lev_names <- level_names_for(maxlev)
  top    <- aggs[aggs$Level == maxlev, , drop = FALSE]
  list(
    ind = ind, aggs = aggs, denoms = denoms, groups = groups,
    maxlev = maxlev, lev_names = lev_names,
    top_code = if (nrow(top)) top$iCode[1] else NA_character_,
    top_name = if (nrow(top)) top$iName[1] else NA_character_,
    children = function(parent) iMeta[!is.na(iMeta$Parent) & iMeta$Parent == parent &
                                        iMeta$Type %in% c("Indicator", "Aggregate"), , drop = FALSE],
    ind_name = function(code) {
      i <- match(code, iMeta$iCode)
      if (is.na(i)) code else iMeta$iName[i]
    }
  )
}

# Validation cross-checks shown on the Data step.
data_crosschecks <- function(iData, iMeta) {
  fw <- fw_info(iData, iMeta)
  ind_codes <- fw$ind$iCode
  matched <- sum(ind_codes %in% names(iData))
  dupes <- sum(duplicated(iData$uCode))
  ntop <- sum(iMeta$Type == "Aggregate" & iMeta$Level == fw$maxlev, na.rm = TRUE)
  ind_cols <- intersect(ind_codes, names(iData))
  nmiss <- sum(is.na(iData[ind_cols]))
  pmiss <- if (length(ind_cols)) 100 * nmiss / (nrow(iData) * length(ind_cols)) else 0
  list(
    list(ok = matched == length(ind_codes),
         text = sprintf("%d / %d indicator codes matched", matched, length(ind_codes))),
    list(ok = dupes == 0, text = sprintf("%d duplicate uCodes", dupes)),
    list(ok = ntop == 1, text = sprintf("hierarchy resolves to %d index", ntop)),
    list(ok = NA,
         text = sprintf("%d missing values (%.1f%%) — handled in Imputation", nmiss, pmiss))
  )
}

# Per-unit data availability on a dset (fraction of non-missing indicator values).
unit_availability <- function(coin, dset, iData) {
  dat <- COINr::get_dset(coin, dset = dset)
  ind_cols <- setdiff(names(dat), c("uCode", "uName", "Time"))
  avail <- rowMeans(!is.na(dat[ind_cols]))
  uName <- iData$uName[match(dat$uCode, iData$uCode)]
  data.frame(uCode = dat$uCode, uName = uName %||% dat$uCode, avail = avail,
             stringsAsFactors = FALSE)
}

# Flagged indicators from coin$Analysis$Treated$Dets_Table after Treat().
treat_flagged <- function(coin, fw) {
  dt <- coin$Analysis$Treated$Dets_Table
  if (is.null(dt) || !"check_SkewKurt0.Pass" %in% names(dt)) return(NULL)
  flg <- dt[!is.na(dt$check_SkewKurt0.Pass) & !dt$check_SkewKurt0.Pass, , drop = FALSE]
  if (!nrow(flg)) return(flg)
  nwin <- if ("winsorise.nwin" %in% names(flg)) flg$winsorise.nwin else rep(NA, nrow(flg))
  skew_after <- if ("check_SkewKurt1.Skew" %in% names(flg)) flg$check_SkewKurt1.Skew else rep(NA, nrow(flg))
  pass_after <- if ("check_SkewKurt1.Pass" %in% names(flg)) flg$check_SkewKurt1.Pass else rep(NA, nrow(flg))
  log_cols <- grep("^log", names(flg), value = TRUE)
  logged <- if (length(log_cols)) apply(flg[log_cols], 1, function(r) any(!is.na(r) & r != "")) else rep(FALSE, nrow(flg))
  how <- ifelse(logged, "Log transform (winsorise insufficient)",
                ifelse(!is.na(nwin) & nwin > 0,
                       paste0("Winsorised ", nwin, ifelse(nwin == 1, " point", " points")),
                       "No treatment applied"))
  data.frame(
    iCode = flg$iCode,
    iName = vapply(flg$iCode, fw$ind_name, ""),
    skew_before = flg$check_SkewKurt0.Skew,
    skew_after = skew_after,
    pass_after = pass_after,
    how = how,
    stringsAsFactors = FALSE
  )
}

# Group column used for the Results filter: the one with fewest distinct values.
pick_group_col <- function(iData, iMeta) {
  gcols <- intersect(iMeta$iCode[iMeta$Type == "Group"], names(iData))
  if (!length(gcols)) return(NULL)
  nuniq <- vapply(gcols, function(g) length(unique(stats::na.omit(iData[[g]]))), 0L)
  gcols[which.min(nuniq)]
}
