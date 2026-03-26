# linkedCells

Reactive cell linking framework for Shiny. Build interactive editable tables where cells update each other through user-defined rules.

Edit a cell, and linked cells rebalance — asynchronously, with undo support, batch editing, and Excel I/O.

## Install

```r
# From GitHub
devtools::install_github("r-rudra/linkedCells")
```

## Quick Start

```r
library(shiny)
library(linkedCells)

ui <- fluidPage(
  linked_cells_ui("table", enable_undo_redo = TRUE)
)

server <- function(input, output, session) {
  linked_cells_server("table", data = my_data, reconcile_fn = my_fn)
}

shinyApp(ui, server)
```

## The Reconciliation Function

When a user edits a cell, your reconciliation function decides what happens next. It receives the full dataframe (with the edit applied) and which cells changed, then returns the updated data.

```r
my_fn <- function(data, edits) {
  # data:  dataframe with user's edit already applied
  # edits: data.frame(row, col) identifying which cells changed

  # Your logic here — rebalance, validate, propagate...

  list(
    data    = updated_data,
    status  = "success",       # "success", "invalid", "not_possible", or "unchanged"
    message = "Rebalanced"     # optional, shown as notification
  )
}
```

If no `reconcile_fn` is provided, the table works as a plain editable table — all edits are accepted as-is, no notifications shown.

## API

```r
# UI
linked_cells_ui(
  id,
  enable_batch_editing = FALSE,
  enable_undo_redo     = FALSE,
  enable_io_buttons    = FALSE
)

# Server
linked_cells_server(
  id,
  data,
  num_locked_rows    = 0,
  reconcile_fn       = NULL,
  tolerance          = 0.1,
  file_name_prefix   = "linked_cells",
  show_notifications = TRUE
)
```

| Parameter | Description |
|---|---|
| `data` | Initial data.frame |
| `num_locked_rows` | Read-only rows at the top |
| `reconcile_fn` | Cell reconciliation function (optional) |
| `tolerance` | Numeric tolerance for change detection |
| `enable_batch_editing` | Buffer edits, commit all at once |
| `enable_undo_redo` | One-step undo / redo |
| `enable_io_buttons` | Download / upload as Excel |
| `file_name_prefix` | Prefix for downloaded filenames |
| `show_notifications` | Show reconciliation toasts (auto-disabled when no `reconcile_fn`) |

## Features

- **Async computation** — reconciliation runs in a background process via `future`. The Shiny UI stays responsive.
- **Batch editing** — buffer multiple cell edits, then commit them together.
- **Undo / Redo** — one-step undo and redo for the last committed change.
- **Excel I/O** — download the current table or upload a modified spreadsheet. Uploads run through reconciliation with change highlighting.
- **Locked rows** — top N rows are read-only (greyed out, not editable).
- **Flash highlighting** — changed cells flash yellow after reconciliation.
- **Column validation on upload** — warns if uploaded file has mismatched columns.

## Example: Row Sum = 100

Each row's numeric columns must sum to 100. Edit one cell and the others rebalance proportionally.

```r
library(shiny)
library(linkedCells)

link_sum_100 <- function(data, edits) {
  numeric_cols <- which(vapply(data, is.numeric, logical(1)))
  edited_rows  <- unique(edits$row)

  for (r in edited_rows) {
    edited_cols <- edits$col[edits$row == r]
    other_cols  <- setdiff(numeric_cols, edited_cols)
    remainder   <- 100 - sum(data[r, edited_cols])

    if (length(other_cols) == 0L) next
    other_sum <- sum(data[r, other_cols])

    if (other_sum <= 0) {
      data[r, other_cols] <- remainder / length(other_cols)
    } else {
      data[r, other_cols] <- data[r, other_cols] * (remainder / other_sum)
    }
  }

  list(data = data, status = "success",
       message = paste("Rebalanced", length(edited_rows), "row(s)"))
}

sample_data <- data.frame(
  Category = paste0("Cat_", 1:5),
  A = c(25, 30, 20, 35, 25),
  B = c(25, 30, 30, 25, 25),
  C = c(25, 20, 30, 20, 25),
  D = c(25, 20, 20, 20, 25)
)

ui <- fluidPage(
  titlePanel("Row Sum = 100"),
  linked_cells_ui("tbl",
    enable_batch_editing = TRUE,
    enable_undo_redo     = TRUE,
    enable_io_buttons    = TRUE
  )
)

server <- function(input, output, session) {
  linked_cells_server("tbl",
    data         = sample_data,
    reconcile_fn = link_sum_100
  )
}

shinyApp(ui, server)
```


## Dependencies

**Imports:** shiny, DT, future, promises, shinyjs, jsonlite

**Suggests:** openxlsx, readxl (for Excel I/O), testthat, later

Requires R >= 4.1.0 (uses native pipe `|>`).

## License

MIT
