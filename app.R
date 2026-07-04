# =========================================================
# COINr Builder — a composite indicator studio built on the
# COINr package (https://bluefoxr.github.io/COINrDoc/).
#
# 13-step wizard: Data → Structure → Denomination → Screening
# → Imputation → Treatment → Normalisation → Weighting →
# Aggregation → Results → Visualisations → Sensitivity → Export,
# with a live R replication-script panel.
#
# Helpers live in R/ (auto-sourced by Shiny).
# =========================================================

library(shiny)
library(COINr)
library(ggplot2)
library(plotly)

quiet <- function(expr) suppressMessages(suppressWarnings(expr))

# run a pipeline step, converting errors into clean messages
pipe_step <- function(label, expr) {
  tryCatch(quiet(expr),
           error = function(e) stop(sprintf("%s: %s", label, conditionMessage(e)), call. = FALSE))
}

# ---------------------------------------------------------
# UI
# ---------------------------------------------------------
ui <- bootstrapPage(
  tags$head(
    tags$title("COINr Builder"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Nunito+Sans:wght@400;700;800;900&display=swap"),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "app.js")
  ),

  div(class = "app-shell",

    # ============ header ============
    div(class = "app-header",
        div(style = "display:flex;align-items:center;gap:10px",
            div(class = "app-logo", "Cr"),
            div(div(class = "app-title", "COINr Builder"),
                div(class = "app-subtitle", "Composite indicator studio"))),
        div(class = "header-sep"),
        div(class = "header-session", uiOutput("session_label", inline = TRUE)),
        div(style = "flex:1"),
        div(class = "busy-pill", "COMPUTING…"),
        div(class = "header-progress", textOutput("progress_text", inline = TRUE)),
        uiOutput("code_toggle_btn", inline = TRUE),
        tags$button(class = "btn-header-white", `data-pick-input` = "nav",
                    `data-pick-value` = "export", "Export")),

    div(class = "app-body",

        # ============ sidebar ============
        div(class = "app-sidebar",
            uiOutput("sidebar_steps"),
            div(class = "side-spacer"),
            div(class = "side-footer",
                "Built on the COINr R package.", tags$br(),
                tags$a(href = "https://bluefoxr.github.io/COINrDoc/", target = "_blank",
                       "COINr documentation ↗"))),

        # ============ main column ============
        div(class = "main-col",
            div(class = "main-scroll", all_step_pages()),
            div(class = "footer-nav", uiOutput("footer_nav"))),

        # ============ R code panel ============
        div(id = "code-panel", class = "code-panel",
            div(class = "code-head",
                div(class = "code-live-dot"),
                div(class = "ttl", "R replication script"),
                div(style = "flex:1"),
                tags$button(class = "btn-code", `data-copy-target` = "#code-raw", "Copy"),
                dl_link("dl_script_panel", ".R ↓", class = "btn-code"),
                tags$button(id = "code_close", class = "action-button btn-code-x", type = "button", "✕")),
            div(class = "code-body", uiOutput("code_lines")),
            div(class = "code-foot", "Updates live as you configure the pipeline."))
    )
  )
)

# ---------------------------------------------------------
# Server
# ---------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(
    step = "data",
    done = character(0),
    built = FALSE,
    code_open = TRUE,
    iData = COINr::ASEM_iData,
    iMeta = COINr::ASEM_iMeta,
    is_example = TRUE,
    idata_name = "ASEM_iData.csv",
    imeta_name = "ASEM_iMeta.csv",
    session_name = "ASEM Sustainable Connectivity 2018",
    sens = NULL,
    agg_error = NULL
  )

  # null/NA-safe input getters
  inp <- function(id, default) input[[id]] %||% default
  inp_num <- function(id, default) {
    v <- input[[id]]
    if (is.null(v) || length(v) == 0 || is.na(v)) default else v
  }

  fw <- reactive(fw_info(rv$iData, rv$iMeta))

  # ======================= navigation =======================
  set_step <- function(id) {
    rv$step <- id
    session$sendCustomMessage("set-step", id)
  }
  observeEvent(input$nav, set_step(input$nav))

  output$sidebar_steps <- renderUI({
    lapply(seq_len(nrow(STEPS)), function(i) {
      s <- STEPS[i, ]
      done <- s$id %in% rv$done
      cur <- s$id == rv$step
      tagList(
        if (!is.na(s$group)) div(class = "side-group", s$group),
        div(class = paste("side-step", if (cur) "current"),
            `data-pick-input` = "nav", `data-pick-value` = s$id,
            div(class = paste("side-dot", if (done) "done"), if (done) "✓" else i),
            div(class = "lbl", s$label))
      )
    })
  })

  output$progress_text <- renderText(sprintf("%d of %d steps saved", length(rv$done), nrow(STEPS)))

  output$session_label <- renderUI({
    if (rv$is_example) {
      tagList(rv$session_name, span(class = "muted", " · example session"))
    } else {
      tagList(rv$session_name, span(class = "muted", " · custom data"))
    }
  })

  output$footer_nav <- renderUI({
    i <- match(rv$step, STEPS$id)
    tagList(
      div(class = "footer-text", sprintf("Step %d of %d — %s", i, nrow(STEPS), STEPS$label[i])),
      div(style = "flex:1"),
      if (i > 1) action_btn("nav_back", "← Back", class = "btn-back"),
      if (i < nrow(STEPS))
        div(class = "footer-continue", action_btn("nav_continue", "Save & continue", class = "btn-primary-red"))
    )
  })
  observeEvent(input$nav_back, {
    i <- match(rv$step, STEPS$id)
    set_step(STEPS$id[max(i - 1, 1)])
  })
  observeEvent(input$nav_continue, {
    i <- match(rv$step, STEPS$id)
    rv$done <- union(rv$done, rv$step)
    set_step(STEPS$id[min(i + 1, nrow(STEPS))])
  })

  # code panel toggle
  output$code_toggle_btn <- renderUI({
    tags$button(id = "code_toggle", type = "button", class = "action-button btn-header-ghost",
                if (rv$code_open) "Hide R code" else "Show R code")
  })
  observeEvent(input$code_toggle, {
    rv$code_open <- !rv$code_open
    session$sendCustomMessage("code-panel", rv$code_open)
  })
  observeEvent(input$code_close, {
    rv$code_open <- FALSE
    session$sendCustomMessage("code-panel", FALSE)
  })

  # ======================= step 1 · data =======================
  output$data_banner <- renderUI({
    if (rv$is_example) {
      banner("Example session — the bundled ASEM 2018 dataset is loaded. Replace either file to start from your own data.")
    } else {
      banner(sprintf("Custom data loaded from %s and %s.", rv$idata_name, rv$imeta_name))
    }
  })

  output$data_files <- renderUI({
    f <- fw()
    ngrp <- sum(f$groups$iCode %in% names(rv$iData))
    file_card <- function(name, meta, btn_id) {
      div(class = "card card-pad",
          div(style = "display:flex;align-items:center;gap:12px",
              div(class = "file-icon", "CSV"),
              div(style = "min-width:0",
                  div(class = "file-name", name),
                  div(class = "file-meta", meta))),
          div(style = "display:flex;align-items:center;gap:10px;margin-top:14px",
              div(class = "check-ok", style = "font-size:12px", "✓ Validated"),
              div(style = "flex:1"),
              action_btn(btn_id, "Replace file")))
    }
    div(class = "file-grid",
        file_card(rv$idata_name,
                  sprintf("%d units × %d indicators · %d denominators · %d groups",
                          nrow(rv$iData), nrow(f$ind), nrow(f$denoms), ngrp),
                  "replace_idata"),
        file_card(rv$imeta_name,
                  sprintf("%d rows · %d levels · directions, weights, denominators",
                          nrow(rv$iMeta), f$maxlev),
                  "replace_imeta"))
  })

  output$data_checks <- renderUI({
    checks <- data_crosschecks(rv$iData, rv$iMeta)
    div(class = "crosschecks",
        lapply(checks, function(ch) {
          if (is.na(ch$ok)) div(span(class = "check-warn", "!"), " ", ch$text)
          else if (ch$ok) div(span(class = "check-ok", "✓"), " ", ch$text)
          else div(span(class = "check-bad", "✗"), " ", ch$text)
        }))
  })

  output$data_preview_title <- renderUI({
    sprintf("Preview — iData (first 6 of %d units)", nrow(rv$iData))
  })

  output$data_preview <- renderUI({
    f <- fw()
    gcol <- pick_group_col(rv$iData, rv$iMeta)
    ind_cols <- head(intersect(f$ind$iCode, names(rv$iData)), 5)
    cols <- c("uCode", "uName", gcol, ind_cols)
    dat <- utils::head(rv$iData[, cols, drop = FALSE], 6)
    tags$table(class = "cb-table",
      tags$thead(tags$tr(lapply(seq_along(cols), function(j)
        th_(toupper(cols[j]), num = cols[j] %in% ind_cols)))),
      tags$tbody(lapply(seq_len(nrow(dat)), function(i) {
        tags$tr(lapply(seq_along(cols), function(j) {
          v <- dat[i, j]
          isnum <- cols[j] %in% ind_cols
          if (is.na(v)) tags$td(class = paste("na-val", if (isnum) "num"), "NA")
          else if (isnum) tags$td(class = "num", format(round(as.numeric(v), 2)))
          else if (j == 1) tags$td(span(class = "code-gray", as.character(v)))
          else tags$td(as.character(v))
        }))
      })))
  })

  # ---- file replacement modal ----
  upload_modal <- function() {
    showModal(modalDialog(
      title = "Replace data files",
      p(style = "font-size:13px;color:var(--gray)",
        "Upload CSV files in COINr format: iData needs uCode + uName columns plus one column per indicator; ",
        "iMeta needs iCode, iName, Level, Direction, Weight, Parent and Type columns. ",
        "You can replace one file or both."),
      fileInput("up_idata", "iData (units × indicators)", accept = ".csv"),
      fileInput("up_imeta", "iMeta (indicator metadata)", accept = ".csv"),
      uiOutput("upload_status"),
      footer = tagList(
        actionButton("load_example", "Load ASEM example", class = "btn btn-default"),
        modalButton("Cancel"),
        actionButton("apply_upload", "Load data", class = "btn btn-primary")
      ), easyClose = TRUE))
  }
  observeEvent(input$replace_idata, upload_modal())
  observeEvent(input$replace_imeta, upload_modal())

  reset_session_state <- function() {
    rv$built <- FALSE
    rv$done <- character(0)
    rv$sens <- NULL
  }

  observeEvent(input$load_example, {
    rv$iData <- COINr::ASEM_iData; rv$iMeta <- COINr::ASEM_iMeta
    rv$is_example <- TRUE
    rv$idata_name <- "ASEM_iData.csv"; rv$imeta_name <- "ASEM_iMeta.csv"
    rv$session_name <- "ASEM Sustainable Connectivity 2018"
    reset_session_state()
    removeModal()
    showNotification("ASEM example data restored.", type = "message")
  })

  observeEvent(input$apply_upload, {
    iData <- rv$iData; iMeta <- rv$iMeta
    idn <- rv$idata_name; imn <- rv$imeta_name
    if (!is.null(input$up_idata)) {
      iData <- try(utils::read.csv(input$up_idata$datapath, check.names = FALSE), silent = TRUE)
      idn <- input$up_idata$name
    }
    if (!is.null(input$up_imeta)) {
      iMeta <- try(utils::read.csv(input$up_imeta$datapath, check.names = FALSE), silent = TRUE)
      imn <- input$up_imeta$name
    }
    err <- NULL
    if (inherits(iData, "try-error")) err <- "Could not read the iData CSV."
    if (is.null(err) && inherits(iMeta, "try-error")) err <- "Could not read the iMeta CSV."
    if (is.null(err)) {
      chk <- tryCatch({ quiet(new_coin(iData, iMeta, quietly = TRUE)); NULL },
                      error = function(e) conditionMessage(e))
      err <- chk
    }
    if (!is.null(err)) {
      output$upload_status <- renderUI(div(class = "banner-error", paste("Validation failed —", err)))
    } else {
      rv$iData <- iData; rv$iMeta <- iMeta
      rv$is_example <- FALSE
      rv$idata_name <- idn; rv$imeta_name <- imn
      rv$session_name <- tools::file_path_sans_ext(idn)
      reset_session_state()
      removeModal()
      showNotification("Data validated and loaded — the pipeline now runs on your files.", type = "message")
    }
  })

  # ======================= step 2 · structure =======================
  output$structure_stats <- renderUI({
    f <- fw()
    chips <- list(div(class = "stat-chip", HTML(sprintf("<b>%d</b> levels", f$maxlev))))
    for (lev in seq(f$maxlev, 1)) {
      n <- if (lev == 1) nrow(f$ind) else sum(f$aggs$Level == lev)
      nm <- tolower(f$lev_names[lev])
      lbl <- if (n == 1) nm else tolower(pluralize_level(nm))
      chips <- c(chips, list(div(class = "stat-chip", HTML(sprintf("<b>%d</b> %s", n, lbl)))))
    }
    div(class = "stat-chips", chips)
  })

  output$framework_ui <- renderUI({
    f <- fw()
    if (is.na(f$top_code)) return(banner("No aggregation hierarchy found in iMeta.", "error"))

    n_ind_under <- function(code) {
      kids <- f$children(code)
      if (!nrow(kids)) return(0)
      sum(vapply(seq_len(nrow(kids)), function(i) {
        if (kids$Type[i] == "Indicator") 1 else n_ind_under(kids$iCode[i])
      }, 0))
    }

    pillar_block <- function(p) {
      kids <- f$children(p$iCode)
      chips <- lapply(kids$iCode, function(cc) div(class = "ind-chip", cc))
      div(class = "fw-pillar",
          div(class = "fw-pillar-head", `data-toggle-next` = "1",
              div(class = "caret", "▸"),
              div(class = "cd", p$iCode),
              div(class = "nm", p$iName),
              div(class = "ct", sprintf("%d %s", nrow(kids),
                  if (all(kids$Type == "Indicator")) "indicators" else "children"))),
          div(class = "fw-chips", chips))
    }

    sub_col <- function(s) {
      pillars <- f$children(s$iCode)
      div(
        div(class = "fw-sub",
            div(class = "lv", sprintf("LEVEL %d", s$Level)),
            div(class = "nm", s$iName),
            div(class = "cd", sprintf("%s · %d ind.", s$iCode, n_ind_under(s$iCode)))),
        lapply(seq_len(nrow(pillars)), function(i) pillar_block(pillars[i, ]))
      )
    }

    subs <- f$children(f$top_code)
    tagList(
      div(class = "fw-banner",
          div(class = "lv", sprintf("LEVEL %d", f$maxlev)),
          div(class = "nm", f$top_name),
          div(class = "cd", f$top_code)),
      div(class = "fw-cols",
          lapply(seq_len(nrow(subs)), function(i) {
            s <- subs[i, ]
            if (s$Type == "Aggregate") sub_col(s)
            else div(class = "fw-pillar", style = "border-top:1px solid var(--border)", pillar_block(s))
          }))
    )
  })

  # ======================= step 3 · denomination =======================
  den_default <- function(code) {
    f <- fw()
    d <- f$ind$Denominator[match(code, f$ind$iCode)]
    if (is.null(d) || is.na(d)) "none" else d
  }
  den_value <- function(code) inp(paste0("den_", code), den_default(code))

  den_selection <- reactive({
    codes <- fw()$ind$iCode
    stats::setNames(vapply(codes, den_value, ""), codes)
  })

  imeta_eff <- reactive({
    iMeta <- rv$iMeta
    sel <- den_selection()
    idx <- iMeta$Type == "Indicator"
    iMeta$Denominator[idx] <- ifelse(sel[iMeta$iCode[idx]] == "none", NA, sel[iMeta$iCode[idx]])
    iMeta
  })

  output$denom_table <- renderUI({
    f <- fw()
    if (!nrow(f$denoms)) {
      return(div(style = "padding:20px", banner("No denominators found in iMeta (rows with Type = 'Denominator'). Add them to your data to enable this step.", "warn")))
    }
    show_all <- isTRUE(input$den_show_all)
    has_def <- !is.na(f$ind$Denominator)
    rows <- if (show_all) f$ind else f$ind[has_def, , drop = FALSE]
    choices <- c("— none —" = "none",
                 stats::setNames(f$denoms$iCode, sprintf("%s (%s)", f$denoms$iName, f$denoms$iCode)))
    tags$table(class = "cb-table",
      tags$thead(tags$tr(th_("Indicator"), th_("Name"), th_("Unit"), th_("Denominator"))),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i) {
        r <- rows[i, ]
        tags$tr(
          tags$td(span(class = "code-red", r$iCode)),
          tags$td(r$iName),
          tags$td(style = "color:var(--gray);font-size:12px", if (is.na(r$Unit)) "—" else r$Unit),
          tags$td(cb_select(paste0("den_", r$iCode), choices, selected = den_value(r$iCode),
                            style = "min-width:190px")))
      })))
  })

  output$denom_summary <- renderUI({
    sel <- den_selection()
    n <- sum(sel != "none")
    ndef <- sum(!is.na(fw()$ind$Denominator))
    sprintf("Showing the %d size-dependent indicators of %d. %d currently assigned a denominator.",
            if (isTRUE(input$den_show_all)) length(sel) else ndef, length(sel), n)
  })

  # ======================= pipeline reactives =======================
  coin0 <- reactive({
    req(rv$iData, rv$iMeta)
    pipe_step("new_coin()", new_coin(rv$iData, imeta_eff(), level_names = fw()$lev_names, quietly = TRUE))
  })

  denominated <- reactive({
    any_den <- any(den_selection() != "none")
    if (!any_den) return(list(coin = coin0(), dset = "Raw"))
    coin <- pipe_step("Denominate()", Denominate(coin0(), dset = "Raw"))
    list(coin = coin, dset = "Denominated")
  })

  avail_df <- reactive(unit_availability(denominated()$coin, denominated()$dset, rv$iData))

  flagged_units <- reactive({
    av <- avail_df()
    av[av$avail < inp_num("screen_thresh", 0.9) - 1e-9, , drop = FALSE]
  })

  force_kept <- reactive({
    fl <- flagged_units()
    fl$uCode[vapply(fl$uCode, function(cc) isTRUE(input[[paste0("force_", cc)]]), TRUE)]
  })

  screened <- reactive({
    d <- denominated()
    kept <- force_kept()
    coin <- pipe_step("Screen()", Screen(
      d$coin, dset = d$dset, unit_screen = "byNA",
      dat_thresh = inp_num("screen_thresh", 0.9),
      Force = if (length(kept)) data.frame(uCode = kept, Include = TRUE)))
    list(coin = coin, dset = "Screened")
  })

  imputed <- reactive({
    s <- screened()
    m <- inp("impute_method", "i_mean_grp")
    if (m == "none") return(s)
    grp_cols <- intersect(fw()$groups$iCode, names(rv$iData))
    if (m == "i_mean_grp") {
      if (!length(grp_cols)) stop("Impute(): no group columns available — pick another imputation method.", call. = FALSE)
      grp <- inp("impute_group", grp_cols[1])
      coin <- pipe_step("Impute()", Impute(s$coin, dset = s$dset, f_i = "i_mean_grp", use_group = grp))
    } else {
      coin <- pipe_step("Impute()", Impute(s$coin, dset = s$dset, f_i = m))
    }
    list(coin = coin, dset = "Imputed")
  })

  treated <- reactive({
    im <- imputed()
    coin <- pipe_step("Treat()", Treat(im$coin, dset = im$dset, global_specs = list(
      f1 = "winsorise",
      f1_para = list(winmax = inp_num("winmax", 5),
                     skew_thresh = inp_num("skew_thresh", 2),
                     kurt_thresh = inp_num("kurt_thresh", 3.5)),
      f2 = "log_CT")))
    list(coin = coin, dset = "Treated")
  })

  normalised <- reactive({
    tr <- treated()
    m <- inp("norm_method", "n_minmax")
    gs <- switch(m,
      n_minmax = list(f_n = "n_minmax", f_n_para = list(l_u = c(inp_num("norm_min", 0), inp_num("norm_max", 100)))),
      n_zscore = list(f_n = "n_zscore", f_n_para = list(m_sd = c(0, 1))),
      list(f_n = m))
    coin <- pipe_step("Normalise()", Normalise(tr$coin, dset = tr$dset, global_specs = gs))
    list(coin = coin, dset = "Normalised")
  })

  # weight slider codes: every aggregate below the top level
  w_codes <- reactive({
    f <- fw()
    f$aggs$iCode[f$aggs$Level < f$maxlev]
  })
  w_default <- function(code) {
    f <- fw()
    w <- f$aggs$Weight[match(code, f$aggs$iCode)]
    if (is.null(w) || is.na(w)) 1 else w
  }
  w_value <- function(code) inp_num(paste0("w_", code), w_default(code))

  w_changes <- reactive({
    codes <- w_codes()
    ch <- vapply(codes, function(cc) w_value(cc), 0)
    names(ch) <- codes
    ch[abs(ch - vapply(codes, w_default, 0)) > 1e-9]
  })

  weights_df <- reactive({
    w <- coin0()$Meta$Weights$Original
    ch <- w_changes()
    for (code in names(ch)) w$Weight[w$iCode == code] <- ch[[code]]
    w
  })

  aggregated <- reactive({
    nr <- normalised()
    f_ag <- inp("agg_method", "a_amean")
    if (f_ag %in% c("a_gmean", "a_hmean")) {
      dat <- COINr::get_dset(nr$coin, dset = "Normalised")
      vals <- dat[setdiff(names(dat), c("uCode", "uName", "Time"))]
      if (any(vals <= 0, na.rm = TRUE)) {
        stop(sprintf("Aggregate(): %s needs strictly positive values — raise the normalisation scale minimum above 0.",
                     method_by_id(AGG_METHODS, f_ag)$title), call. = FALSE)
      }
    }
    coin <- pipe_step("Aggregate()", Aggregate(nr$coin, dset = "Normalised", f_ag = f_ag, w = weights_df()))
    list(coin = coin, dset = "Aggregated")
  })

  base_ranks <- reactive({
    coin <- pipe_step("Aggregate()", Aggregate(normalised()$coin, dset = "Normalised",
                                               f_ag = "a_amean", w = coin0()$Meta$Weights$Original))
    res <- quiet(get_results(coin, dset = "Aggregated", tab_type = "Aggs"))
    stats::setNames(res$Rank, res$uCode)
  })

  results_df <- reactive({
    res <- quiet(get_results(aggregated()$coin, dset = "Aggregated", tab_type = "Aggs",
                             also_get = "uName", nround = 1))
    gcol <- pick_group_col(rv$iData, rv$iMeta)
    res$Group <- if (!is.null(gcol)) as.character(rv$iData[[gcol]][match(res$uCode, rv$iData$uCode)]) else ""
    res
  })

  # ======================= step 4 · screening =======================
  output$screen_summary <- renderUI({
    fl <- flagged_units()
    sprintf("%d units flagged · %d screened out", nrow(fl), nrow(fl) - length(force_kept()))
  })

  output$screen_flagged <- renderUI({
    fl <- flagged_units()
    if (!nrow(fl)) {
      return(div(style = "padding:16px 20px;font-size:13px;font-weight:700;color:var(--green)",
                 "✓ No units fall below the threshold."))
    }
    fl <- fl[order(fl$avail), ]
    tags$table(class = "cb-table", style = "margin-top:6px",
      tags$thead(tags$tr(th_("Unit", style = "padding-left:20px"), th_("Data availability"),
                         th_("Status"), th_("Keep anyway"))),
      tags$tbody(lapply(seq_len(nrow(fl)), function(i) {
        r <- fl[i, ]
        kept <- isTRUE(input[[paste0("force_", r$uCode)]])
        pct <- round(r$avail * 100)
        tags$tr(
          tags$td(style = "padding-left:20px", span(class = "code-gray", r$uCode),
                  HTML("&nbsp;&nbsp;"), tags$b(r$uName)),
          tags$td(div(style = "display:flex;align-items:center;gap:10px",
                      div(class = "avail-bar", div(style = sprintf("width:%d%%", pct))),
                      div(style = "font-size:12.5px;font-weight:700", paste0(pct, "%")))),
          tags$td(span(style = sprintf("font-size:12px;font-weight:800;color:%s",
                                       if (kept) "var(--green)" else "var(--red)"),
                       if (kept) "KEPT (FORCED)" else "SCREENED OUT")),
          tags$td(cb_check(paste0("force_", r$uCode), kept)))
      })))
  })

  # ======================= step 5 · imputation =======================
  output$impute_desc <- renderUI({
    f <- fw()
    ind_cols <- intersect(f$ind$iCode, names(rv$iData))
    nmiss <- sum(is.na(rv$iData[ind_cols]))
    pmiss <- 100 * nmiss / (nrow(rv$iData) * max(length(ind_cols), 1))
    nunits <- sum(rowSums(is.na(rv$iData[ind_cols])) > 0)
    sprintf("%d values (%.1f%%) are missing across %d units. Choose how to fill them — or leave them missing and let aggregation handle gaps.",
            nmiss, pmiss, nunits)
  })

  output$impute_cards <- renderUI(
    method_cards("impute_method", IMP_METHODS, inp("impute_method", "i_mean_grp")))

  output$impute_group_ui <- renderUI({
    f <- fw()
    grp_cols <- intersect(f$groups$iCode, names(rv$iData))
    if (!length(grp_cols)) return(banner("No group columns found in the data.", "warn"))
    labs <- sprintf("%s — %s", grp_cols, f$groups$iName[match(grp_cols, f$groups$iCode)])
    cb_select("impute_group", stats::setNames(grp_cols, labs),
              selected = inp("impute_group", grp_cols[length(grp_cols)]), class = "cb-select wide")
  })

  # ======================= step 6 · treatment =======================
  treat_rows <- reactive(treat_flagged(treated()$coin, fw()))

  output$treat_title <- renderUI({
    tr <- treat_rows()
    sprintf("Flagged indicators — %d of %d exceed thresholds",
            if (is.null(tr)) 0 else nrow(tr), nrow(fw()$ind))
  })

  output$treat_table <- renderUI({
    tr <- treat_rows()
    if (is.null(tr) || !nrow(tr)) {
      return(div(style = "padding:16px 20px;font-size:13px;font-weight:700;color:var(--green)",
                 "✓ No indicators exceed the skew and kurtosis thresholds."))
    }
    tags$table(class = "cb-table", style = "margin-top:6px",
      tags$thead(tags$tr(th_("Indicator", style = "padding-left:20px"),
                         th_("Skew before", num = TRUE), th_("Skew after", num = TRUE),
                         th_("Treatment applied"))),
      tags$tbody(lapply(seq_len(nrow(tr)), function(i) {
        r <- tr[i, ]
        tags$tr(
          tags$td(style = "padding-left:20px", span(class = "code-red", r$iCode),
                  HTML("&nbsp;&nbsp;"), r$iName),
          tags$td(class = "num", style = "font-weight:700", fmt1(r$skew_before)),
          tags$td(class = "num", style = "color:var(--green);font-weight:800",
                  if (is.na(r$skew_after)) "—" else fmt1(r$skew_after)),
          tags$td(style = "font-size:12.5px", r$how))
      })))
  })

  # ======================= step 7 · normalisation =======================
  output$norm_cards <- renderUI(
    method_cards("norm_method", NORM_METHODS, inp("norm_method", "n_minmax")))

  output$dist_ind_ui <- renderUI({
    codes <- fw()$ind$iCode
    tr <- treat_rows()
    def <- if (!is.null(tr) && nrow(tr)) tr$iCode[1] else codes[1]
    cb_select("dist_ind", codes, selected = inp("dist_ind", def))
  })

  dist_sel <- reactive({
    codes <- fw()$ind$iCode
    sel <- inp("dist_ind", codes[1])
    if (sel %in% codes) sel else codes[1]
  })

  output$dist_fn <- renderUI(
    sprintf('plot_dist(coin, dset = "Normalised", iCodes = "%s", type = "Histogram")', dist_sel()))

  dist_theme <- theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())

  output$norm_plot_before <- renderPlot({
    tr <- treated()
    quiet(plot_dist(tr$coin, dset = tr$dset, iCodes = dist_sel(), type = "Histogram")) +
      labs(title = paste0(dist_sel(), " — treated (before)")) + dist_theme
  }, res = 96)

  output$norm_plot_after <- renderPlot({
    nr <- normalised()
    quiet(plot_dist(nr$coin, dset = "Normalised", iCodes = dist_sel(), type = "Histogram")) +
      labs(title = paste0(dist_sel(), " — normalised (after)")) + dist_theme
  }, res = 96)

  # ======================= step 8 · weighting =======================
  output$weights_summary <- renderUI({
    n <- length(w_changes())
    if (n) sprintf("%d weights differ from original", n) else "All weights at original values"
  })

  # slider moved -> flip the preset select to "custom"
  observe({
    ch <- w_changes()
    if (length(ch) && identical(inp("weight_preset", "original"), "original")) {
      session$sendCustomMessage("set-vals", list(list(id = "weight_preset", value = "custom")))
    }
  })
  # preset set back to "original" -> reset sliders
  observeEvent(input$weight_preset, {
    if (identical(input$weight_preset, "original") && length(w_changes())) {
      session$sendCustomMessage("set-vals",
        lapply(w_codes(), function(cc) list(id = paste0("w_", cc), value = w_default(cc))))
    }
  })

  output$weights_ui <- renderUI({
    f <- fw()
    if (!nrow(f$aggs)) return(banner("No aggregation hierarchy found.", "error"))

    w_row <- function(code, name, siblings, small = FALSE) {
      vals <- vapply(siblings, w_value, 0)
      pct <- round(w_value(code) / max(sum(vals), 1e-9) * 100)
      div(class = "w-row",
          div(class = paste("lbl", if (small) "sm"), name),
          cb_range(paste0("w_", code), 0, 2, 0.05, w_value(code)),
          if (!small) div(class = "w-val", `data-live-for` = paste0("w_", code),
                          `data-live-mode` = "fix2", fmt2(w_value(code))),
          div(class = paste("w-pct", if (small) "plain"), paste0(pct, "%")))
    }

    group_card <- function(parent_code, parent_name, child_level, full = FALSE) {
      kids <- f$children(parent_code)
      kids <- kids[kids$Type == "Aggregate", , drop = FALSE]
      if (!nrow(kids)) return(NULL)
      ttl <- sprintf("%s → %s", pluralize_level(f$lev_names[child_level]), parent_name)
      div(class = "card", style = "padding:18px 20px;margin-bottom:14px",
          div(class = "card-title", style = "margin-bottom:12px", ttl),
          lapply(kids$iCode, function(cc)
            w_row(cc, f$ind_name(cc), kids$iCode, small = !full)))
    }

    top_card <- group_card(f$top_code, f$top_name, f$maxlev - 1, full = TRUE)
    subs <- f$aggs[f$aggs$Level == f$maxlev - 1, , drop = FALSE]
    sub_cards <- lapply(seq_len(nrow(subs)), function(i)
      group_card(subs$iCode[i], subs$iName[i], f$maxlev - 2))
    sub_cards <- Filter(Negate(is.null), sub_cards)

    tagList(
      top_card,
      if (length(sub_cards)) div(class = "pillar-w-grid", sub_cards),
      div(style = "font-size:12px;color:var(--gray);margin-top:12px",
          "Indicator-level weights keep their iMeta values; edit them in the exported script if needed.")
    )
  })

  output$rank_preview <- renderUI({
    res <- tryCatch(results_df(), error = function(e) e)
    if (inherits(res, "error")) {
      return(div(style = "color:#FF8A97;font-size:12.5px;line-height:1.5", conditionMessage(res)))
    }
    base <- tryCatch(base_ranks(), error = function(e) NULL)
    top <- utils::head(res[order(res$Rank), ], 8)
    lapply(seq_len(nrow(top)), function(i) {
      r <- top[i, ]
      d <- if (is.null(base)) 0 else (base[[r$uCode]] %||% r$Rank) - r$Rank
      div(class = "rank-row",
          div(class = "rk", r$Rank),
          div(class = "nm", r$uName),
          div(class = "sc", fmt1(r[[fw()$top_code]])),
          div(class = "dl", style = sprintf("color:%s",
              if (d > 0) "#7BD389" else if (d < 0) "#FF8A97" else "#93A5BB"),
              if (d > 0) paste0("▲", d) else if (d < 0) paste0("▼", -d) else "—"))
    })
  })

  # ======================= step 9 · aggregation =======================
  output$agg_desc <- renderUI({
    f <- fw()
    HTML(sprintf("Roll indicator scores up the hierarchy: %s.",
                 paste(tolower(f$lev_names), collapse = " → ")))
  })

  output$agg_cards <- renderUI(
    method_cards("agg_method", AGG_METHODS, inp("agg_method", "a_amean")))

  do_build <- function() {
    res <- tryCatch({ aggregated(); NULL }, error = function(e) conditionMessage(e))
    if (is.null(res)) {
      rv$built <- TRUE
      rv$agg_error <- NULL
      rv$done <- union(rv$done, "aggregate")
      showNotification("Index built — the Aggregated data set was appended to the coin.", type = "message")
    } else {
      rv$agg_error <- res
      showNotification(res, type = "error", duration = 8)
    }
  }
  observeEvent(input$build_btn, do_build())
  observeEvent(input$build_btn2, do_build())
  observeEvent(input$build_btn3, do_build())

  output$agg_banner <- renderUI({
    if (!is.null(rv$agg_error)) {
      return(div(style = "margin-top:18px", banner(rv$agg_error, "error")))
    }
    if (!rv$built) return(NULL)
    am <- method_by_id(AGG_METHODS, inp("agg_method", "a_amean"))
    div(style = "margin-top:18px",
        banner(sprintf("Index built with %s — Aggregated data set appended to the coin. Explore it under Results.",
                       tolower(am$title))))
  })

  # keep the aggregation refreshed once built (so downstream errors surface early)
  observe({
    if (rv$built) {
      err <- tryCatch({ aggregated(); NULL }, error = function(e) conditionMessage(e))
      rv$agg_error <- err
    }
  })

  # ======================= step 10 · results =======================
  empty_state <- function(btn_id, txt) {
    div(class = "empty-state",
        div(class = "ttl", "No aggregated scores yet"),
        div(class = "txt", txt),
        div(style = "width:230px", action_btn(btn_id, "Build the index", class = "btn-primary-red")))
  }

  output$results_body <- renderUI({
    if (!rv$built) {
      return(empty_state("build_btn2",
        "Build the index with your current pipeline settings, then explore ranks and scores here."))
    }
    gcol <- pick_group_col(rv$iData, rv$iMeta)
    gvals <- if (!is.null(gcol)) sort(unique(stats::na.omit(as.character(rv$iData[[gcol]])))) else character(0)
    tagList(
      div(class = "res-toolbar",
          cb_text("res_search", placeholder = "Search units…", value = inp("res_search", "")),
          if (length(gvals))
            cb_select("res_group", c("All groups" = "All", stats::setNames(gvals, gvals)),
                      selected = inp("res_group", "All"), style = "padding:9px 12px"),
          div(style = "flex:1"),
          div(style = "font-size:12.5px;color:var(--gray)", uiOutput("res_count", inline = TRUE)),
          dl_link("dl_csv2", "Download CSV")),
      div(class = "card", uiOutput("results_table"))
    )
  })

  filtered_results <- reactive({
    res <- results_df()
    q <- tolower(trimws(inp("res_search", "")))
    g <- inp("res_group", "All")
    if (g != "All") res <- res[res$Group == g, , drop = FALSE]
    if (nzchar(q)) {
      res <- res[grepl(q, tolower(res$uName), fixed = TRUE) |
                 grepl(q, tolower(res$uCode), fixed = TRUE), , drop = FALSE]
    }
    res
  })

  output$res_count <- renderUI({
    sprintf("%d of %d units", nrow(filtered_results()), nrow(results_df()))
  })

  output$results_table <- renderUI({
    res <- tryCatch(filtered_results(), error = function(e) e)
    if (inherits(res, "error")) return(div(style = "padding:20px", banner(conditionMessage(res), "error")))
    f <- fw()
    subs <- f$children(f$top_code)
    sub_codes <- subs$iCode[subs$Type == "Aggregate"]
    all_res <- results_df()
    max_idx <- max(all_res[[f$top_code]], na.rm = TRUE)
    tagList(
      tags$table(class = "cb-table",
        tags$thead(tags$tr(
          th_("Rank", style = "width:56px;padding-left:18px"), th_("Unit"), th_("Group", style = "width:90px"),
          th_(toupper(f$top_code), style = "width:260px"),
          lapply(sub_codes, function(sc) th_(toupper(sc), num = TRUE, style = "width:80px")))),
        tags$tbody(lapply(seq_len(nrow(res)), function(i) {
          r <- res[i, ]
          idx <- r[[f$top_code]]
          tags$tr(
            tags$td(style = "padding-left:18px;font-weight:800;color:var(--gray)", r$Rank),
            tags$td(tags$b(r$uName), HTML("&nbsp;&nbsp;"),
                    span(style = "font-family:var(--mono);font-size:11.5px;color:var(--gray)", r$uCode)),
            tags$td(style = "font-size:12px;color:var(--gray)", r$Group),
            tags$td(div(style = "display:flex;align-items:center;gap:10px",
                        div(class = "score-bar", div(style = sprintf("width:%d%%", round(idx / max_idx * 100)))),
                        div(style = "font-family:var(--mono);font-size:12.5px;font-weight:700;width:42px;text-align:right",
                            fmt1(idx)))),
            lapply(sub_codes, function(sc)
              tags$td(class = "num", style = "font-family:var(--mono);font-size:12.5px", fmt1(r[[sc]]))))
        }))),
      div(class = "card-note",
          sprintf("Scores computed by COINr from %d units with your current pipeline settings.", nrow(all_res)))
    )
  })

  # ======================= step 11 · visualisations =======================
  output$viz_body <- renderUI({
    if (!rv$built) {
      return(empty_state("build_btn3", "Build the index first — charts are drawn from the Aggregated data set."))
    }
    f <- fw()
    tagList(div(class = "viz-grid",
      div(class = "card", style = "padding:18px 20px",
          div(class = "card-title", style = "margin-bottom:0", "Index scores — top 12"),
          div(class = "plot-fn", style = "padding:0 0 14px",
              sprintf('plot_bar(coin, dset = "Aggregated", iCode = "%s")', f$top_code)),
          uiOutput("viz_bars")),
      div(class = "viz-col",
          div(class = "plot-frame",
              div(class = "plot-title", "Choropleth map"),
              div(class = "plot-fn", "scores by uCode (ISO3)"),
              plotlyOutput("viz_map", height = 200)),
          div(class = "plot-frame",
              div(class = "plot-title", "Correlation heatmap"),
              div(class = "plot-fn", 'plot_corr(coin, dset = "Normalised", grouplev = 2)'),
              plotOutput("viz_corr", height = 260)),
          div(class = "plot-frame",
              div(class = "plot-title", "Framework sunburst"),
              div(class = "plot-fn", 'plot_framework(coin, type = "sunburst")'),
              plotOutput("viz_sunburst", height = 260)))
    ))
  })

  output$viz_bars <- renderUI({
    res <- tryCatch(results_df(), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    f <- fw()
    top <- utils::head(res[order(res$Rank), ], 12)
    max_idx <- max(res[[f$top_code]], na.rm = TRUE)
    lapply(seq_len(nrow(top)), function(i) {
      r <- top[i, ]
      div(class = "hbar-row",
          div(class = "nm", r$uName),
          div(class = "bar", div(style = sprintf("width:%d%%", round(r[[f$top_code]] / max_idx * 100)))),
          div(class = "sc", fmt1(r[[f$top_code]])))
    })
  })

  output$viz_map <- renderPlotly({
    req(rv$built)
    res <- results_df()
    f <- fw()
    plot_ly(res, type = "choropleth", locations = ~uCode, z = res[[f$top_code]],
            text = ~uName,
            colorscale = list(list(0, "#EAF1F8"), list(1, "#D0021B")),
            marker = list(line = list(color = "#FFFFFF", width = 0.5))) |>
      colorbar(title = NULL, thickness = 8, len = 0.8) |>
      layout(geo = list(showframe = FALSE, showcoastlines = FALSE,
                        projection = list(type = "natural earth"),
                        bgcolor = "rgba(0,0,0,0)"),
             margin = list(l = 0, r = 0, t = 0, b = 0),
             paper_bgcolor = "rgba(0,0,0,0)") |>
      config(displayModeBar = FALSE)
  })

  output$viz_corr <- renderPlot({
    req(rv$built)
    f <- fw()
    quiet(plot_corr(aggregated()$coin, dset = "Normalised",
                    grouplev = min(2, max(f$maxlev - 1, 1)), showvals = FALSE)) +
      theme(axis.text = element_text(size = 6), legend.position = "none")
  }, res = 96)

  output$viz_sunburst <- renderPlot({
    quiet(plot_framework(coin0(), type = "sunburst"))
  }, res = 96)

  # ======================= step 12 · sensitivity =======================
  observeEvent(input$run_sens, {
    if (!rv$built) {
      showNotification("Build the index first (Aggregation step) — sensitivity re-runs the full pipeline.",
                       type = "warning")
      return()
    }
    vary_w <- isTRUE(inp("sens_w", TRUE))
    vary_norm <- isTRUE(inp("sens_norm", FALSE))
    vary_agg <- isTRUE(inp("sens_agg", TRUE))
    if (!any(vary_w, vary_norm, vary_agg)) {
      showNotification("Tick at least one assumption to vary.", type = "warning")
      return()
    }
    N <- inp_num("sens_n", 500)
    coin <- tryCatch(aggregated()$coin, error = function(e) NULL)
    if (is.null(coin)) {
      showNotification("The pipeline has an error — fix it before running the analysis.", type = "error")
      return()
    }
    SA_specs <- list()
    if (vary_w) {
      SA_specs$Weights <- list(
        Address = "$Log$Aggregate$w",
        Distribution = quiet(get_noisy_weights(w = weights_df(),
          noise_specs = data.frame(Level = 2, NoiseFactor = 0.25), Nrep = N)),
        Type = "discrete", Name = "Weights")
    }
    if (vary_norm) {
      SA_specs$Normalisation <- list(
        Address = "$Log$Normalise$global_specs",
        Distribution = list(
          list(f_n = "n_minmax", f_n_para = list(l_u = c(inp_num("norm_min", 0), inp_num("norm_max", 100)))),
          list(f_n = "n_zscore", f_n_para = list(m_sd = c(50, 10)))),
        Type = "discrete", Name = "Normalisation")
    }
    if (vary_agg) {
      SA_specs$Aggregation <- list(
        Address = "$Log$Aggregate$f_ag",
        Distribution = c("a_amean", "a_gmean"),
        Type = "discrete", Name = "Aggregation")
    }
    withProgress(message = sprintf("Re-building the index %d times…", N), value = 0.3, {
      SA <- tryCatch({
        utils::capture.output(out <- quiet(get_sensitivity(
          coin, SA_specs = SA_specs, N = N, SA_type = "UA",
          dset = "Aggregated", iCode = fw()$top_code,
          quietly = TRUE, check_addresses = FALSE)))
        out
      }, error = function(e) e)
      setProgress(1)
    })
    if (inherits(SA, "error")) {
      showNotification(paste("get_sensitivity():", conditionMessage(SA)), type = "error", duration = 8)
      return()
    }
    rv$sens <- list(SA = SA, N = N, w = vary_w, norm = vary_norm, agg = vary_agg)
    rv$done <- union(rv$done, "sensitivity")
  })

  output$sens_results <- renderUI({
    s <- rv$sens
    if (is.null(s)) return(NULL)
    rs <- s$SA$RankStats
    n_units <- nrow(rs)
    rs$spread <- rs$Q95 - rs$Q5
    top <- utils::head(rs[order(-rs$spread), ], 8)
    top <- top[order(top$Nominal), ]
    uname <- rv$iData$uName[match(top$uCode, rv$iData$uCode)]
    varied <- paste(c(if (s$w) "weights", if (s$norm) "normalisation", if (s$agg) "aggregation"),
                    collapse = ", ")
    div(class = "card", style = "padding:18px 22px",
        div(class = "card-title", style = "margin-bottom:0", "Rank confidence intervals — most uncertain units"),
        div(style = "font-size:12px;color:var(--gray);margin-bottom:14px",
            sprintf("%d replications · assumptions varied: %s", s$N, varied)),
        lapply(seq_len(nrow(top)), function(i) {
          r <- top[i, ]
          lo <- max(1, floor(r$Q5)); hi <- min(n_units, ceiling(r$Q95))
          div(class = "ci-row",
              div(class = "nm", uname[i] %||% r$uCode),
              div(class = "ci-track",
                  div(class = "ci-band", style = sprintf("left:%.1f%%;width:%.1f%%",
                      (lo - 1) / n_units * 100, (hi - lo + 1) / n_units * 100)),
                  div(class = "ci-nominal", style = sprintf("left:%.1f%%",
                      (r$Nominal - 1) / n_units * 100))),
              div(class = "ci-range", sprintf("#%d  [%d–%d]", r$Nominal, lo, hi)))
        }),
        div(style = "font-size:11.5px;color:var(--gray);margin-top:12px",
            sprintf("Red mark = nominal rank · grey band = 90%% interval across replications · axis spans ranks 1–%d.", n_units)))
  })

  # ======================= step 13 · export & codegen =======================
  settings_list <- reactive({
    f <- fw()
    orig_den <- f$ind$Denominator
    names(orig_den) <- f$ind$iCode
    sel <- den_selection()
    changed <- names(sel)[ifelse(sel == "none", !is.na(orig_den), is.na(orig_den) | orig_den != sel)]
    den_changes <- lapply(stats::setNames(changed, changed),
                          function(cc) if (sel[[cc]] == "none") NA_character_ else sel[[cc]])
    xlsx <- paste0(gsub("[^A-Za-z0-9]+", "_", rv$session_name), "_index.xlsx")
    list(
      session_name = rv$session_name,
      is_example = rv$is_example,
      idata_file = rv$idata_name, imeta_file = rv$imeta_name,
      lev_names = f$lev_names,
      den_changes = den_changes,
      any_denom = any(sel != "none"),
      thresh = inp_num("screen_thresh", 0.9),
      force_kept = force_kept(),
      impute_method = inp("impute_method", "i_mean_grp"),
      impute_group = inp("impute_group", utils::tail(intersect(f$groups$iCode, names(rv$iData)), 1) %||% ""),
      winmax = inp_num("winmax", 5), skew = inp_num("skew_thresh", 2), kurt = inp_num("kurt_thresh", 3.5),
      norm_method = inp("norm_method", "n_minmax"),
      norm_min = inp_num("norm_min", 0), norm_max = inp_num("norm_max", 100),
      w_changes = as.list(w_changes()),
      agg_method = inp("agg_method", "a_amean"),
      sens = if (!is.null(rv$sens)) rv$sens[c("N", "w", "norm", "agg")],
      xlsx_name = xlsx
    )
  })

  script_lines <- reactive(build_script(settings_list()))

  output$code_lines <- renderUI({
    lines <- script_lines()
    tagList(
      lapply(lines, function(l) {
        div(class = paste("code-line", if (grepl("^\\s*#", l)) "comment"),
            if (nzchar(l)) l else HTML("&nbsp;"))
      }),
      tags$pre(id = "code-raw", style = "display:none", paste(lines, collapse = "\n"))
    )
  })

  output$script_meta <- renderUI(sprintf("%d lines of commented R.", length(script_lines())))

  output$export_summary <- renderUI({
    s <- settings_list()
    f <- fw()
    imp <- method_by_id(IMP_METHODS, s$impute_method)
    nrm <- method_by_id(NORM_METHODS, s$norm_method)
    agm <- method_by_id(AGG_METHODS, s$agg_method)
    fl <- tryCatch(flagged_units(), error = function(e) NULL)
    nexcl <- if (is.null(fl)) 0 else nrow(fl) - length(s$force_kept)
    rows <- list(
      c("Data", sprintf("%s — %d units, %d indicators, %d levels",
                        s$session_name, nrow(rv$iData), nrow(f$ind), f$maxlev)),
      c("Denomination", sprintf("%d indicators denominated (%s)",
                                sum(den_selection() != "none"),
                                if (nrow(f$denoms)) paste(tolower(f$denoms$iName), collapse = ", ") else "none available")),
      c("Screening", sprintf("%d%% availability threshold — %d excluded, %d kept manually",
                             round(s$thresh * 100), nexcl, length(s$force_kept))),
      c("Imputation", paste0(imp$title, if (s$impute_method == "i_mean_grp") sprintf(" (%s)", s$impute_group) else "")),
      c("Treatment", sprintf("Winsorise up to %s pts — skew %s, kurtosis %s", s$winmax, s$skew, s$kurt)),
      c("Normalisation", paste0(nrm$title, if (s$norm_method == "n_minmax") sprintf(" [%s, %s]", s$norm_min, s$norm_max) else "")),
      c("Weights", if (length(s$w_changes)) sprintf("Custom — %d weights adjusted", length(s$w_changes)) else "Original (equal)"),
      c("Aggregation", paste0(agm$title, if (rv$built) " — index built" else " — not built yet")),
      c("Sensitivity", if (!is.null(s$sens)) sprintf("Uncertainty analysis, N = %d", s$sens$N) else "Not run")
    )
    lapply(rows, function(r) div(class = "summary-row", div(class = "k", r[1]), div(r[2])))
  })

  # ---- downloads ----
  csv_handler <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", rv$session_name), "_results.csv"),
    content = function(file) {
      res <- tryCatch(results_df(), error = function(e) NULL)
      if (is.null(res)) res <- data.frame(note = "Build the index first (Aggregation step).")
      utils::write.csv(res, file, row.names = FALSE)
    })
  output$dl_csv <- csv_handler
  output$dl_csv2 <- csv_handler

  script_handler <- downloadHandler(
    filename = function() paste0(gsub("[^A-Za-z0-9]+", "_", rv$session_name), "_COINr_replication.R"),
    content = function(file) writeLines(script_lines(), file))
  output$dl_script <- script_handler
  output$dl_script_panel <- script_handler

  output$dl_xlsx <- downloadHandler(
    filename = function() settings_list()$xlsx_name,
    content = function(file) {
      coin <- tryCatch(aggregated()$coin, error = function(e) NULL)
      if (is.null(coin)) coin <- tryCatch(normalised()$coin, error = function(e) NULL)
      validate(need(!is.null(coin), "Pipeline error — fix settings before exporting."))
      quiet(export_to_excel(coin, fname = file))
    })
}

shinyApp(ui, server)
