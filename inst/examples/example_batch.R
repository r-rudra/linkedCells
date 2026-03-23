# ============================================================
# linkedCells Example: Batch Mode Budget
# ============================================================

library(shiny)
library(DT)
devtools::load_all(".")

# Linking Function
link_budget <- function(data, edits) {
  
  # Validate all edits
  for (i in seq_len(nrow(edits))) {
    row_idx <- edits$row[i]
    col_idx <- edits$col[i]
    new_val <- as.numeric(data[row_idx, col_idx])
    
    if (is.na(new_val) || new_val < 0) {
      return(list(data = data, status = "invalid", message = sprintf("Row %d: Invalid", row_idx)))
    }
  }
  
  # Apply edits
  for (i in seq_len(nrow(edits))) {
    row_idx <- edits$row[i]
    col_idx <- edits$col[i]
    data[row_idx, col_idx] <- as.numeric(data[row_idx, col_idx])
  }
  
  # Check budget constraint
  numeric_cols <- which(sapply(data, is.numeric))
  for (row_idx in unique(edits$row)) {
    row_total <- sum(data[row_idx, numeric_cols], na.rm = TRUE)
    if (row_total > 1000) {
      return(list(data = data, status = "not_possible", message = sprintf("Row %d exceeds $1000", row_idx)))
    }
  }
  
  return(list(data = data, status = "success", message = sprintf("Updated %d cells", nrow(edits))))
}

# Data
budget_data <- data.frame(
  Project = c("ProjectA", "ProjectB", "ProjectC", "ProjectD"),
  Marketing = c(200, 150, 300, 250),
  Operations = c(300, 350, 200, 400),
  RnD = c(500, 500, 500, 350),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("linkedCells: Batch Mode Budget"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Batch Mode"),
      p("1. Enable batch mode"),
      p("2. Edit multiple cells"),
      p("3. Click Commit"),
      hr(),
      verbatimTextOutput("totals")
    ),
    mainPanel(width = 9, linked_cells_ui("budget_table"))
  )
)

# Server
server <- function(input, output, session) {
  reactive_data <- linked_cells_server("budget_table", budget_data, 0, link_budget)
  
  output$totals <- renderPrint({
    data <- reactive_data()
    numeric_cols <- which(sapply(data, is.numeric))
    totals <- rowSums(data[, numeric_cols], na.rm = TRUE)
    data.frame(Project = data$Project, Total = totals)
  })
}

shinyApp(ui, server)
