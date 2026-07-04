# shinycoin — COINr Builder

A Shiny app for building composite indices with the [COINr](https://bluefoxr.github.io/COINrDoc/) package — a 13-step "composite indicator studio" that walks you from raw data to a finished, exportable index.

## What it does

The app wraps the full COINr pipeline in a guided wizard:

| Phase | Steps | COINr functions |
|---|---|---|
| **Build** | Data · Structure · Denomination · Screening · Imputation · Treatment · Normalisation · Weighting · Aggregation | `new_coin()` `Denominate()` `Screen()` `Impute()` `Treat()` `Normalise()` `Aggregate()` |
| **Explore** | Results · Visualisations · Sensitivity | `get_results()` `plot_dist()` `plot_corr()` `plot_framework()` `get_sensitivity()` |
| **Deliver** | Export | `export_to_excel()` + generated replication script |

Key features:

- **Live R replication script** — a side panel shows the exact COINr code reproducing your session, updating as you change settings. Copy it or download it as a `.R` file that runs top-to-bottom with only COINr installed.
- **Real computation** — every control re-runs the actual COINr pipeline (cached step by step), including a live ranking preview while you drag weight sliders.
- **Bring your own data** — starts with the bundled ASEM 2018 example; replace iData/iMeta with your own CSVs (validated via `new_coin()` before loading).
- **Uncertainty analysis** — perturb weights, normalisation and aggregation with `get_sensitivity()` and see rank confidence intervals.
- **Exports** — results CSV, replication script, and the full coin as an Excel workbook.

## Running

```r
install.packages(c("shiny", "COINr", "ggplot2", "plotly"))
shiny::runApp()
```

## Layout

```
app.R            # UI shell + server (pipeline reactives, outputs)
R/components.R   # design tokens as UI helpers, method/step definitions
R/pipeline.R     # framework metadata, availability & treatment helpers
R/codegen.R      # replication-script generator
R/steps_ui.R     # static UI for the 13 wizard steps
www/styles.css   # design system (navy #193351, red #D0021B, Nunito Sans)
www/app.js       # native range-slider binding, step switching, clipboard
```

The interface was designed in [Claude Design](https://claude.ai/design) using the British Red Cross component library and implemented as plain Shiny + custom CSS/JS (no bslib theme).
