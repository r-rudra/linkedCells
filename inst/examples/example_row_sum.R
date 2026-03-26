# ============================================================
# linkedCells Example: Row Sum = 100 (With Async Test Input)
# ============================================================

library(shiny)
devtools::load_all(".")

# ------------------------------------------------------------------
# Linking function: rebalance edited rows so numeric columns sum to 100.
# ------------------------------------------------------------------
link_sum_100 <- function(data, edits) {

  numeric_cols <- which(vapply(data, is.numeric, logical(1)))
  edited_rows  <- unique(edits$row)

  for (r in edited_rows) {

    edited_cols <- edits$col[edits$row == r]

    for (ec in edited_cols) {
      val <- as.numeric(data[r, ec])
      if (is.na(val)) {
        return(list(data = data, status = "invalid",
                    message = sprintf("Row %d: must be numeric", r)))
      }
      if (val < 0 || val > 100) {
        return(list(data = data, status = "invalid",
                    message = sprintf("Row %d: must be 0-100", r)))
      }
    }

    other_cols <- setdiff(numeric_cols, edited_cols)
    edited_sum <- sum(data[r, edited_cols], na.rm = TRUE)
    other_sum  <- sum(data[r, other_cols],  na.rm = TRUE)
    remainder  <- 100 - edited_sum

    if (length(other_cols) == 0L) next

    if (other_sum <= 0) {
      data[r, other_cols] <- remainder / length(other_cols)
    } else {
      data[r, other_cols] <- data[r, other_cols] * (remainder / other_sum)
    }
  }

  Sys.sleep(5)

  list(data = data, status = "success",
       message = paste("Rebalanced", length(edited_rows), "row(s)"))
}

# ------------------------------------------------------------------
# Sample data
# ------------------------------------------------------------------
sample_data <- data.frame(
  Category = paste0("Cat_", 1:10),
  ColA = c(20, 25, 15, 30, 18, 22, 28, 19, 25, 30),
  ColB = c(30, 25, 35, 25, 32, 28, 25, 31, 35, 28),
  ColC = c(30, 30, 30, 30, 35, 30, 30, 30, 25, 27),
  ColD = c(20, 20, 20, 15, 15, 20, 17, 20, 15, 15),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------
# UI  (flags live here only -- server auto-detects)
# ------------------------------------------------------------------
ui <- shiny::fluidPage(
  shiny::titlePanel("linkedCells: Row Sum = 100"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      shiny::h4("Edit rows 9-10"),
      shiny::p("Rows 1-8 are locked. Edit any numeric cell in rows 9 or 10."),
      shiny::p("The other columns auto-adjust so the row still sums to 100."),
      shiny::hr(),

      # --- NEW: Dummy Async Test Input ---
      shiny::h4("Async Test"),
      shiny::numericInput("dummy_num", "Enter a number:", value = 5),
      shiny::verbatimTextOutput("dummy_square_out"),
      shiny::hr(),
      # -----------------------------------

      shiny::verbatimTextOutput("row_sums")
    ),
    shiny::mainPanel(
      width = 9,
      linked_cells_ui(
        "main_table",
        enable_batch_editing = TRUE,
        enable_undo_redo     = TRUE,
        enable_io_buttons = TRUE
      )
    )
  )
)

# ------------------------------------------------------------------
# Server  (no flags needed -- UI controls what appears)
# ------------------------------------------------------------------
server <- function(input, output, session) {

  # --- NEW: Dummy Async Test Logic ---
  output$dummy_square_out <- shiny::renderPrint({
    shiny::req(input$dummy_num)
    result <- input$dummy_num ^ 2
    paste("Square :", result)
  })
  # -----------------------------------

  reactive_data <- linked_cells_server(
    "main_table",
    data            = sample_data,
    num_locked_rows = 8,
    reconcile_fn    = link_sum_100
  )

  output$row_sums <- shiny::renderPrint({
    d <- reactive_data()
    numeric_cols <- which(vapply(d, is.numeric, logical(1)))
    sums <- rowSums(d[, numeric_cols], na.rm = TRUE)
    data.frame(Row = seq_len(nrow(d)), Sum = round(sums, 2))
  })
}

shiny::shinyApp(ui, server)
