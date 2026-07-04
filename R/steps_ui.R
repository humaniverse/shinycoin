# =========================================================
# COINr Builder — static UI for the 13 wizard steps.
# Dynamic parts (anything derived from the loaded data) are
# uiOutputs / plotOutputs filled in by the server.
# =========================================================

step_page <- function(id, ..., width = NULL) {
  div(class = paste("step-page", if (id == "data") "active"), `data-step` = id,
      div(class = paste("step-wrap", width), ...))
}

ui_step_data <- function() {
  step_page("data",
    step_header("data", "new_coin()", "Data",
      HTML("A coin is assembled from two tables: <b>iData</b> (one row per unit, one column per indicator) and <b>iMeta</b> (one row per indicator, defining the hierarchy, directions and weights).")),
    uiOutput("data_banner"),
    uiOutput("data_files"),
    div(class = "card", style = "padding:16px 20px;margin-bottom:16px",
        div(class = "card-title", style = "margin-bottom:10px", "Cross-checks"),
        uiOutput("data_checks")),
    div(class = "card", style = "overflow:hidden",
        div(style = "padding:14px 20px 0;font-weight:800;font-size:13.5px", uiOutput("data_preview_title", inline = TRUE)),
        div(style = "overflow-x:auto;padding:8px 8px 12px", uiOutput("data_preview")))
  )
}

ui_step_structure <- function() {
  step_page("structure",
    step_header("structure", "iMeta · plot_framework()", "Index structure",
      "The hierarchy defined in iMeta. Click a pillar to inspect its indicators. Weights are edited later, in the Weighting step."),
    uiOutput("structure_stats"),
    div(class = "card", style = "padding:20px 22px", uiOutput("framework_ui"))
  )
}

ui_step_denominate <- function() {
  step_page("denominate",
    step_header("denominate", "Denominate()", "Denomination",
      HTML("Size-dependent indicators are divided by a measure of country size, so that large economies don't dominate. Assignments are stored in iMeta's <span class='mono'>Denominator</span> column.")),
    div(class = "card",
        uiOutput("denom_table"),
        div(class = "card-note",
            tags$label(style = "display:flex;align-items:center;gap:8px;font-weight:400;margin:0 0 6px;cursor:pointer",
                  cb_check("den_show_all", FALSE), "Show all indicators (assign denominators beyond the iMeta defaults)"),
            uiOutput("denom_summary", inline = TRUE)))
  )
}

ui_step_screen <- function() {
  step_page("screen",
    step_header("screen", "Screen()", "Unit screening",
      "Remove units with too little data to score fairly. Flagged units can still be kept individually."),
    div(class = "card", style = "padding:20px 22px;margin-bottom:16px",
        div(class = "card-title", style = "margin-bottom:4px", "Minimum data availability"),
        div(style = "font-size:12.5px;color:var(--gray);margin-bottom:12px",
            "Units with fewer non-missing indicator values than this are screened out."),
        div(style = "display:flex;align-items:center;gap:16px",
            cb_range("screen_thresh", 0.5, 1, 0.01, 0.9, style = "width:340px"),
            live_label("screen_thresh", "90%", mode = "pct"),
            div(style = "font-size:12.5px;color:var(--gray)", uiOutput("screen_summary", inline = TRUE)))),
    div(class = "card",
        div(style = "padding:14px 20px 0;font-weight:800;font-size:13.5px", "Flagged units"),
        uiOutput("screen_flagged"))
  )
}

ui_step_impute <- function() {
  step_page("impute",
    step_header("impute", "Impute()", "Imputation", uiOutput("impute_desc", inline = TRUE)),
    uiOutput("impute_cards"),
    conditionalPanel(
      condition = "(input.impute_method || 'i_mean_grp') == 'i_mean_grp'",
      div(class = "card", style = "padding:16px 20px;max-width:420px",
          div(class = "field-label", style = "margin-bottom:8px", "Grouping variable"),
          uiOutput("impute_group_ui"),
          div(style = "font-size:12px;color:var(--gray);margin-top:8px",
              "Missing values are replaced by the mean of units in the same group — usually more defensible than a global mean.")))
  )
}

ui_step_treat <- function() {
  step_page("treat",
    step_header("treat", "Treat()", "Outlier treatment",
      "Indicators whose skew and kurtosis exceed the thresholds are winsorised point by point; if that isn't enough, a log transform is applied instead."),
    div(class = "card", style = "padding:20px 22px;margin-bottom:16px;display:flex;gap:34px;flex-wrap:wrap;align-items:flex-end",
        div(div(class = "field-label", "Skew threshold"), cb_num("skew_thresh", 2, step = 0.1)),
        div(div(class = "field-label", "Kurtosis threshold"), cb_num("kurt_thresh", 3.5, step = 0.1)),
        div(div(class = "field-label", "Max winsorised points ", span(class = "field-hint", "winmax")),
            div(style = "display:flex;align-items:center;gap:12px",
                cb_range("winmax", 0, 10, 1, 5, style = "width:200px"),
                live_label("winmax", "5", style = "font-size:18px;font-weight:800;width:24px")))),
    div(class = "card",
        div(style = "padding:14px 20px 0;font-weight:800;font-size:13.5px", uiOutput("treat_title", inline = TRUE)),
        uiOutput("treat_table"),
        div(class = "card-note",
            "Directions of treatment respect each indicator's direction in iMeta. Re-run happens automatically when thresholds change."))
  )
}

ui_step_normalise <- function() {
  step_page("normalise",
    step_header("normalise", "Normalise()", "Normalisation",
      "Bring all indicators onto a common scale, respecting each one's direction (higher = better, or reversed)."),
    uiOutput("norm_cards"),
    conditionalPanel(
      condition = "(input.norm_method || 'n_minmax') == 'n_minmax'",
      div(class = "card", style = "padding:16px 20px;display:flex;gap:26px;align-items:flex-end;max-width:420px;margin-bottom:16px",
          div(div(class = "field-label", "Scale minimum"), cb_num("norm_min", 0)),
          div(div(class = "field-label", "Scale maximum"), cb_num("norm_max", 100)))),
    div(class = "card", style = "padding:16px 20px",
        div(style = "display:flex;align-items:center;gap:14px;margin-bottom:8px",
            div(class = "card-title", style = "margin:0", "Distribution preview — before / after"),
            uiOutput("dist_ind_ui", inline = TRUE)),
        div(class = "plot-fn", uiOutput("dist_fn", inline = TRUE)),
        div(style = "display:grid;grid-template-columns:1fr 1fr;gap:14px",
            plotOutput("norm_plot_before", height = 240),
            plotOutput("norm_plot_after", height = 240)))
  )
}

ui_step_weights <- function() {
  step_page("weights", width = "xwide",
    step_header("weights", "coin$Meta$Weights", "Weighting",
      "Weights are relative within each aggregation group. Drag a slider and watch the ranking react on the right."),
    div(class = "weights-grid",
        div(
          div(class = "card", style = "padding:16px 20px;margin-bottom:14px;display:flex;align-items:center;gap:14px",
              div(class = "field-label", style = "margin:0", "Weight set"),
              cb_select("weight_preset",
                        c("Original (equal, from iMeta)" = "original", "Custom" = "custom"),
                        selected = "original"),
              div(style = "font-size:12px;color:var(--gray)", uiOutput("weights_summary", inline = TRUE))),
          uiOutput("weights_ui")),
        div(class = "rank-preview",
            div(class = "ttl", "Live ranking preview"),
            div(class = "sub", "Top 8 · change vs original weights"),
            uiOutput("rank_preview"),
            div(class = "rank-note", "Uses current normalisation and aggregation settings.")))
  )
}

ui_step_aggregate <- function() {
  step_page("aggregate",
    step_header("aggregate", "Aggregate()", "Aggregation",
      uiOutput("agg_desc", inline = TRUE)),
    uiOutput("agg_cards"),
    div(style = "display:flex;align-items:center;gap:16px",
        div(style = "width:250px", action_btn("build_btn", "Build composite index", class = "btn-primary-red")),
        div(style = "font-size:12.5px;color:var(--gray)", "Appends the Aggregated data set to the coin.")),
    uiOutput("agg_banner")
  )
}

ui_step_results <- function() {
  step_page("results", width = "wide",
    step_header("results", "get_results()", "Results", ""),
    uiOutput("results_body")
  )
}

ui_step_viz <- function() {
  step_page("viz", width = "wide",
    step_header("viz", "plot_bar() · plot_corr() · plot_framework()", "Visualisations", ""),
    uiOutput("viz_body")
  )
}

ui_step_sensitivity <- function() {
  step_page("sensitivity",
    step_header("sensitivity", "get_sensitivity()", "Sensitivity & uncertainty",
      "Re-build the index many times under perturbed assumptions to see how stable the ranking is."),
    div(class = "card", style = "padding:20px 22px;margin-bottom:16px",
        div(class = "card-title", style = "margin-bottom:12px", "Assumptions to vary"),
        tags$label(class = "sens-check", cb_check("sens_w", TRUE),
                   span(HTML("<b>Weights</b> — perturb pillar weights ±25%"))),
        tags$label(class = "sens-check", cb_check("sens_norm", FALSE),
                   span(HTML("<b>Normalisation</b> — alternate min-max / z-score"))),
        tags$label(class = "sens-check", cb_check("sens_agg", TRUE),
                   span(HTML("<b>Aggregation</b> — alternate arithmetic / geometric mean"))),
        div(style = "display:flex;align-items:center;gap:16px;margin-top:14px;padding-top:14px;border-top:1px solid var(--border)",
            div(class = "field-label", style = "margin:0", "Replications"),
            cb_range("sens_n", 100, 2000, 100, 500, style = "width:280px"),
            live_label("sens_n", "500", style = "font-size:16px;font-weight:800;width:52px"),
            div(style = "flex:1"),
            div(style = "width:210px", action_btn("run_sens", "Run analysis", class = "btn-primary-red"))),
        div(style = "font-size:12px;color:var(--gray);margin-top:10px",
            "Runs take roughly 5–10 seconds per 100 replications. If the aggregation alternative includes the geometric mean, set the normalisation scale minimum above 0 to avoid failed replications.")),
    uiOutput("sens_results")
  )
}

ui_step_export <- function() {
  step_page("export",
    step_header("export", "export_to_excel()", "Export",
      "Take your index with you — as data, and as an R script that reproduces every step of this session with COINr."),
    div(class = "export-grid",
        div(class = "export-card",
            div(class = "icon", style = "background:var(--green)", "CSV"),
            div(class = "ttl", "Results table"),
            div(class = "txt", "Ranks and scores at every level for all units, with your current settings applied."),
            dl_link("dl_csv", "Download .csv", class = "btn-primary-red")),
        div(class = "export-card",
            div(class = "icon", style = "background:var(--navy);font-size:11px", ".R"),
            div(class = "ttl", "Replication script"),
            div(class = "txt", uiOutput("script_meta", inline = TRUE),
                " Runs top-to-bottom in R with only COINr installed."),
            dl_link("dl_script", "Download .R script", class = "btn-primary-red")),
        div(class = "export-card",
            div(class = "icon", style = "background:var(--teal)", "XLSX"),
            div(class = "ttl", "Excel workbook"),
            div(class = "txt", "The full coin — every data set, results and metadata — via export_to_excel()."),
            dl_link("dl_xlsx", "Download .xlsx", class = "btn-primary-red"))),
    div(class = "card", style = "padding:18px 22px",
        div(class = "card-title", style = "margin-bottom:12px", "Session summary"),
        uiOutput("export_summary"))
  )
}

all_step_pages <- function() {
  tagList(
    ui_step_data(), ui_step_structure(), ui_step_denominate(), ui_step_screen(),
    ui_step_impute(), ui_step_treat(), ui_step_normalise(), ui_step_weights(),
    ui_step_aggregate(), ui_step_results(), ui_step_viz(), ui_step_sensitivity(),
    ui_step_export()
  )
}
