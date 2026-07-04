# =========================================================
# COINr Builder — UI components & constants
# =========================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ---- pipeline steps (order defines the wizard) ----
STEPS <- data.frame(
  id    = c("data", "structure", "denominate", "screen", "impute", "treat",
            "normalise", "weights", "aggregate", "results", "viz", "sensitivity", "export"),
  label = c("Data", "Index structure", "Denomination", "Unit screening", "Imputation",
            "Outlier treatment", "Normalisation", "Weighting", "Aggregation",
            "Results", "Visualisations", "Sensitivity", "Export"),
  group = c("BUILD", NA, NA, NA, NA, NA, NA, NA, NA, "EXPLORE", NA, NA, "DELIVER"),
  stringsAsFactors = FALSE
)

step_group <- function(id) {
  i <- match(id, STEPS$id)
  grp <- STEPS$group[seq_len(i)]
  tail(grp[!is.na(grp)], 1)
}

# ---- method definitions ----
IMP_METHODS <- list(
  list(id = "none",       title = "No imputation",    fn = "",           desc = "Keep gaps. Aggregation averages over available values only."),
  list(id = "i_mean",     title = "Indicator mean",   fn = "i_mean",     desc = "Replace each gap with the indicator's mean across all units."),
  list(id = "i_median",   title = "Indicator median", fn = "i_median",   desc = "Replace with the median — more robust to outliers."),
  list(id = "i_mean_grp", title = "Group mean",       fn = "i_mean_grp", desc = "Replace with the mean of the unit's own group (e.g. Asia / Europe).")
)
NORM_METHODS <- list(
  list(id = "n_minmax", title = "Min-max", fn = "n_minmax", desc = "Linear rescale onto a fixed interval. The most common choice."),
  list(id = "n_zscore", title = "Z-score", fn = "n_zscore", desc = "Centre on the mean, scale by standard deviation."),
  list(id = "n_rank",   title = "Rank",    fn = "n_rank",   desc = "Replace values by their ranks. Removes all distribution shape."),
  list(id = "n_borda",  title = "Borda",   fn = "n_borda",  desc = "Borda scores: rank minus one. Used in voting-style aggregation.")
)
AGG_METHODS <- list(
  list(id = "a_amean",  title = "Arithmetic mean", fn = "a_amean",  desc = "Weighted average. Fully compensatory — strengths offset weaknesses."),
  list(id = "a_gmean",  title = "Geometric mean",  fn = "a_gmean",  desc = "Penalises unbalanced profiles. Requires strictly positive values."),
  list(id = "a_hmean",  title = "Harmonic mean",   fn = "a_hmean",  desc = "Even less compensatory than geometric. Sensitive to low scores."),
  list(id = "a_median", title = "Median",          fn = "a_median", desc = "Middle value of the children. Ignores weights; very robust.")
)

method_by_id <- function(defs, id) defs[[which(vapply(defs, function(m) m$id, "") == id)]]

# ---- low-level inputs (raw HTML so Shiny's default bindings pick them up) ----
cb_select <- function(id, choices, selected = NULL, class = "cb-select", style = NULL) {
  labs <- names(choices) %||% unname(unlist(choices))
  if (is.null(names(choices))) labs <- unname(unlist(choices))
  vals <- unname(unlist(choices))
  opts <- lapply(seq_along(vals), function(i) {
    tags$option(
      value = vals[i],
      selected = if (!is.null(selected) && vals[i] == selected) NA else NULL,
      if (nzchar(labs[i])) labs[i] else vals[i]
    )
  })
  tags$select(id = id, class = class, style = style, opts)
}

cb_range <- function(id, min, max, step, value, style = NULL) {
  tags$input(type = "range", id = id, class = "cb-range",
             min = min, max = max, step = step, value = value, style = style)
}

live_label <- function(for_id, text, mode = "raw", style = "font-size:20px;font-weight:800;min-width:64px") {
  tags$div(`data-live-for` = for_id, `data-live-mode` = mode, style = style, text)
}

cb_num <- function(id, value, step = NULL) {
  tags$input(type = "number", id = id, class = "cb-num", value = value, step = step)
}

cb_check <- function(id, checked = FALSE) {
  tags$input(type = "checkbox", id = id, class = "cb-check",
             checked = if (checked) NA else NULL)
}

cb_text <- function(id, placeholder = "", value = "") {
  tags$input(type = "text", id = id, class = "cb-text", placeholder = placeholder, value = value)
}

action_btn <- function(id, label, class = "btn-outline", style = NULL) {
  tags$button(id = id, type = "button", class = paste("action-button", class), style = style, label)
}

dl_link <- function(id, label, class = "btn-outline") {
  tags$a(id = id, class = paste("shiny-download-link", class), href = "", target = "_blank", download = NA, label)
}

# ---- composite pieces ----
step_header <- function(step_id, fn_chip, title, desc) {
  n <- match(step_id, STEPS$id)
  tagList(
    div(class = "step-kicker-row",
        div(class = "step-kicker", sprintf("STEP %d OF %d · %s", n, nrow(STEPS), step_group(step_id))),
        if (nzchar(fn_chip)) div(class = "fn-chip", fn_chip)),
    div(class = "step-title", title),
    div(class = "step-desc", desc)
  )
}

banner <- function(text, variant = "info") {
  div(class = paste0("banner-", variant), text)
}

method_cards <- function(input_id, defs, selected) {
  div(class = "method-grid",
      lapply(defs, function(m) {
        div(class = paste("method-card", if (m$id == selected) "selected"),
            `data-pick-input` = input_id, `data-pick-value` = m$id,
            div(class = "method-radio", div(class = "dot")),
            div(
              div(class = "method-title", m$title,
                  if (nzchar(m$fn)) span(class = "field-hint", m$fn)),
              div(class = "method-desc", m$desc)
            ))
      }))
}

# "Index" -> "Indices", "Sub-index" -> "Sub-indices", "Pillar" -> "Pillars"
pluralize_level <- function(nm) {
  if (grepl("ex$", nm)) sub("ex$", "ices", nm)
  else if (grepl("x$", nm)) paste0(nm, "es")
  else paste0(nm, "s")
}

# table header helper
th_ <- function(label, num = FALSE, style = NULL) {
  tags$th(class = if (num) "num" else NULL, style = style, label)
}

fmt1 <- function(x) formatC(x, format = "f", digits = 1)
fmt2 <- function(x) formatC(x, format = "f", digits = 2)
