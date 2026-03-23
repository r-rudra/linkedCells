# ============================================================
# R/module.R
# linkedCells: Core Shiny module (UI + Server)
# ============================================================

#' LinkedCells UI Component
#'
#' Creates the user interface for a linked cells table.
#'
#' @param id Module ID (character string)
#'
#' @export
linked_cells_ui <- function(id) {

  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML(sprintf("
        #%s_controls {
          padding: 12px;
          background: #f5f5f5;
          border-bottom: 1px solid #ddd;
          margin-bottom: 12px;
          border-radius: 4px;
        }

        #%s_table .linked-edited {
          background-color: #FFEB3B !important;
          font-weight: bold;
        }

        #%s_table .linked-adjusted {
          background-color: #FFE5B4 !important;
        }

        #%s_busy {
          display: none;
          position: fixed;
          top: 50%%;
          left: 50%%;
          transform: translate(-50%%, -50%%);
          background: rgba(0,0,0,0.85);
          color: white;
          padding: 25px 35px;
          border-radius: 6px;
          z-index: 9999;
          font-size: 16px;
        }

        #%s_busy.show {
          display: block;
        }
      ", ns("controls"), ns("table"), ns("table"), ns("busy"), ns("busy")))
    ),

    div(
      id = ns("controls"),
      fluidRow(
        column(
          3,
          checkboxInput(ns("batch_mode"), "Batch edit mode", value = FALSE)
        ),
        column(
          3,
          uiOutput(ns("commit_button_ui"))
        ),
        column(
          6,
          textOutput(ns("status_text")),
          style = "text-align: right; padding-top: 7px;"
        )
      )
    ),

    DTOutput(ns("table")),

    div(id = ns("busy"), "⏳ Processing cell links...")
  )
}

#' LinkedCells Server Module
#'
#' Server logic for linked cells table.
#'
#' @param id Module ID
#' @param data Initial data frame
#' @param num_locked_rows Number of locked rows (default: 0)
#' @param link_fn User-defined linking function: function(data, edits)
#' @param tolerance Numeric tolerance for change detection (default: 0.1)
#'
#' @return Reactive data frame containing current state
#'
#' @export
linked_cells_server <- function(id, data, num_locked_rows = 0, link_fn, tolerance = 0.1) {

  moduleServer(id, function(input, output, session) {

    # Initialize async
    ensure_async_ready()

    # State management
    state <- reactiveValues(
      committed = data,
      in_editor = data,
      is_computing = FALSE,
      exec_token = 0,
      active_token = 0,
      last_status = "ready",
      last_message = ""
    )

    table_proxy <- dataTableProxy("table", session = session)

    # Commit button UI
    output$commit_button_ui <- renderUI({
      if (isTRUE(input$batch_mode)) {
        actionButton(
          session$ns("commit"),
          if (state$is_computing) "Processing..." else "Commit Changes",
          class = if (state$is_computing) "btn-warning btn-sm" else "btn-primary btn-sm",
          disabled = state$is_computing
        )
      }
    })

    # Render table
    output$table <- renderDT({
      display_data <- if (isTRUE(input$batch_mode)) state$in_editor else state$committed

      datatable(
        display_data,
        rownames = FALSE,
        selection = "none",
        editable = list(
          target = "cell",
          disable = list(rows = seq_len(num_locked_rows))
        ),
        options = list(
          dom = 't',
          ordering = FALSE,
          paging = FALSE,
          info = FALSE,
          searching = FALSE
        )
      ) %>%
        formatRound(columns = which(sapply(display_data, is.numeric)), digits = 2)
    })

    # Sync UI when committed data changes
    observeEvent(state$committed, {
      display_data <- if (isTRUE(input$batch_mode)) state$in_editor else state$committed
      replaceData(table_proxy, display_data, resetPaging = FALSE)
    }, ignoreInit = TRUE)

    # Sync UI when editor buffer changes
    observeEvent(state$in_editor, {
      if (isTRUE(input$batch_mode)) {
        replaceData(table_proxy, state$in_editor, resetPaging = FALSE)
      }
    }, ignoreInit = TRUE)

    # Handle cell edits
    observeEvent(input$table_cell_edit, {

      info <- input$table_cell_edit
      row_idx <- info$row
      col_idx <- info$col + 1
      new_value <- info$value

      # Guard: Locked row
      if (row_idx <= num_locked_rows) {
        showNotification("This row is read-only", type = "warning", duration = 2)
        replaceData(table_proxy, state$committed)
        return()
      }

      # Guard: Computing
      if (state$is_computing) {
        showNotification("Processing... Please wait", type = "info", duration = 2)
        replaceData(table_proxy, if (isTRUE(input$batch_mode)) state$in_editor else state$committed)
        return()
      }

      # Batch mode: Store in buffer
      if (isTRUE(input$batch_mode)) {
        buf <- state$in_editor
        buf[row_idx, col_idx] <- new_value
        state$in_editor <- buf

        state$last_status <- "buffered"
        state$last_message <- sprintf("Row %d queued", row_idx)

        replaceData(table_proxy, buf)
        return()
      }

      # Reactive mode: Process immediately
      state$is_computing <- TRUE
      addClass(session$ns("busy"), "show")

      data_with_edit <- state$committed
      data_with_edit[row_idx, col_idx] <- new_value

      edits <- make_edits_df(row_idx, col_idx)

      exec_token <- generate_token()
      state$active_token <- exec_token

      # Execute async
      with_async_safety(
        {
          link_fn(data_with_edit, edits)
        },
        session = session
      ) %...>%
        (function(result) {

          if (!is_token_active(exec_token, state$active_token)) {
            state$is_computing <- FALSE
            return()
          }

          if (!is.list(result) || !("data" %in% names(result)) || !("status" %in% names(result))) {
            state$is_computing <- FALSE
            state$last_status <- "error"
            showNotification("Invalid link_fn result", type = "error")
            replaceData(table_proxy, state$committed)
            return()
          }

          state$last_status <- result$status
          state$last_message <- result$message %||% ""

          # Process based on status
          switch(result$status,
            "success" = {
              state$committed <- result$data
              state$in_editor <- result$data
              showNotification(paste("✓", state$last_message), type = "message", duration = 2)
            },
            "invalid" = {
              showNotification(paste("⚠", result$message %||% ""), type = "warning", duration = 3)
              state$in_editor <- state$committed
            },
            "not_possible" = {
              showNotification(paste("✗", result$message %||% ""), type = "error", duration = 3)
              state$in_editor <- state$committed
            },
            "unchanged" = {
              showNotification("No changes", type = "info", duration = 2)
              state$in_editor <- state$committed
            }
          )

          state$is_computing <- FALSE
          removeClass(session$ns("busy"), "show")
        })
    })

    # Commit button
    observeEvent(input$commit, {

      if (!isTRUE(input$batch_mode) || state$is_computing) return()

      changes <- detect_changes(state$committed, state$in_editor, num_locked_rows, tolerance)

      if (nrow(changes) == 0) {
        showNotification("No changes", type = "info", duration = 2)
        return()
      }

      state$is_computing <- TRUE
      addClass(session$ns("busy"), "show")

      exec_token <- generate_token()
      state$active_token <- exec_token

      with_async_safety(
        {
          link_fn(state$in_editor, changes)
        },
        session = session
      ) %...>%
        (function(result) {

          if (!is_token_active(exec_token, state$active_token)) {
            state$is_computing <- FALSE
            return()
          }

          if (!is.list(result) || !("data" %in% names(result))) {
            state$is_computing <- FALSE
            showNotification("Error", type = "error")
            replaceData(table_proxy, state$committed)
            return()
          }

          state$last_status <- result$status

          if (result$status == "success") {
            state$committed <- result$data
            state$in_editor <- result$data
            showNotification(sprintf("✓ Committed %d changes", nrow(changes)), type = "message", duration = 2)
          } else {
            showNotification(paste("Failed:", result$message %||% ""), type = "error", duration = 3)
            state$in_editor <- state$committed
          }

          state$is_computing <- FALSE
          removeClass(session$ns("busy"), "show")
        })
    })

    # Status display
    output$status_text <- renderText({
      emoji <- switch(state$last_status, "ready" = "✓", "success" = "✓", "buffered" = "📝", "invalid" = "⚠", "error" = "✗", "?")
      color <- switch(state$last_status, "success" = "#4CAF50", "buffered" = "#2196F3", "invalid" = "#FF9800", "error" = "#f44336", "#666")
      msg <- if (nchar(state$last_message) > 0) sprintf("%s %s (%s)", emoji, state$last_status, state$last_message) else sprintf("%s %s", emoji, state$last_status)
      sprintf('<span style="color: %s; font-size: 0.9em;">%s</span>', color, msg)
    })

    # Return reactive data
    return(reactive(state$committed))
  })
}

# Null coalescing
`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x)) y else x
}
