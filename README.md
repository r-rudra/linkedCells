# linkedCells - Reactive Cell Linking Framework

A Shiny framework for building interactive, constraint-based editable tables.

## Quick Setup

1. Extract the ZIP file
2. `devtools::load_all("linkedCells")`
3. `source("linkedCells/inst/examples/example_row_sum.R")`

## API

```r
linked_cells_init()           # Initialize async (optional)
linked_cells_ui("id")         # Create UI
linked_cells_server(          # Create server
  id, 
  data,                       # Your dataframe
  num_locked_rows = 0,        # Read-only rows
  link_fn = your_function,    # Your linking logic
  tolerance = 0.1             # Float tolerance
)
```

## Your Linking Function

```r
my_link_fn <- function(data, edits) {
  # data: dataframe with user's edit applied
  # edits: data.frame(row=..., col=...) of edited cells
  
  # Your logic to update linked cells
  
  return(list(
    data = updated_data,
    status = "success",  # or "invalid" or "not_possible"
    message = "Optional message"
  ))
}
```

## Files

- `R/` - Core code (4 files)
- `inst/examples/` - Working examples (2 apps)
- `DESCRIPTION` - Package metadata
- `NAMESPACE` - Exports
- `LICENSE` - MIT

## Example Usage

```r
library(shiny)
devtools::load_all("linkedCells")

# Define linking function
link_sum_100 <- function(data, edits) {
  row <- edits$row[1]
  col <- edits$col[1]
  val <- as.numeric(data[row, col])
  
  if (val < 0 || val > 100) {
    return(list(data = data, status = "invalid"))
  }
  
  data[row, col] <- val
  # Rebalance other cells...
  
  list(data = data, status = "success")
}

# Create app
ui <- fluidPage(linked_cells_ui("table"))
server <- function(input, output, session) {
  linked_cells_server("table", my_data, 8, link_sum_100)
}

shinyApp(ui, server)
```

## Status

Production-ready, CRAN-compliant framework.
Version 1.0.0 | License: MIT
