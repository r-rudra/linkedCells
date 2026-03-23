# ============================================================
# linkedCells Example: Row Sum = 100
# ============================================================

library(shiny)
library(DT)
devtools::load_all(".")

# Linking Function
link_sum_100 <- function(data, edits) {

  Sys.sleep(5)
  row_idx <- edits$row[1]
  col_idx <- edits$col[1]
  new_value <- as.numeric(data[row_idx, col_idx])

  if (is.na(new_value)) {
    return(list(data = data, status = "invalid", message = "Must be numeric"))
  }

  if (new_value < 0 || new_value > 100) {
    return(list(data = data, status = "invalid", message = "Must be 0-100"))
  }

  data[row_idx, col_idx] <- new_value

  numeric_cols <- which(sapply(data, is.numeric))
  other_cols <- setdiff(numeric_cols, col_idx)
  other_sum <- sum(data[row_idx, other_cols], na.rm = TRUE)

  if (other_sum <= 0) {
    data[row_idx, other_cols] <- (100 - new_value) / length(other_cols)
  } else {
    scale_factor <- (100 - new_value) / other_sum
    data[row_idx, other_cols] <- data[row_idx, other_cols] * scale_factor
  }

  return(list(data = data, status = "success", message = "Rebalanced"))
}

# Data
sample_data <- data.frame(
  Category = paste0("Cat_", 1:10),
  ColA = c(20, 25, 15, 30, 18, 22, 28, 19, 25, 30),
  ColB = c(30, 25, 35, 25, 32, 28, 25, 31, 35, 28),
  ColC = c(30, 30, 30, 30, 35, 30, 30, 30, 25, 27),
  ColD = c(20, 20, 20, 15, 15, 20, 17, 20, 15, 15),
  stringsAsFactors = FALSE
)

# UI
ui <- fluidPage(
  titlePanel("linkedCells: Row Sum = 100"),
  sidebarLayout(
    sidebarPanel(width = 3,
      h4("Edit rows 9-10"),
      p("Other columns auto-adjust"),
      hr(),
      verbatimTextOutput("row_sums")
    ),
    mainPanel(width = 9, linked_cells_ui("main_table"))
  )
)

# Server
server <- function(input, output, session) {
  reactive_data <- linked_cells_server("main_table", sample_data, 8, link_sum_100)

  output$row_sums <- renderPrint({
    data <- reactive_data()
    numeric_cols <- which(sapply(data, is.numeric))
    sums <- rowSums(data[, numeric_cols], na.rm = TRUE)
    data.frame(Row = 1:nrow(data), Sum = round(sums, 2))
  })
}

shinyApp(ui, server)
