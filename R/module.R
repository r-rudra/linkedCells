# ============================================================
# R/module.R
# linkedCells: Core Shiny module (UI + Server)
# ============================================================

#' LinkedCells UI Component
#'
#' @param id Module ID (character string)
#'
#' @export
linked_cells_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shinyjs::useShinyjs(),

    shiny::tags$head(
      shiny::tags$style(shiny::HTML(sprintf("
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
      ", ns("controls"), ns("table"), ns("table"), ns("busy"), ns("busy"))))
    ),

    shiny::div(
      id = ns("controls"),
      shiny::fluidRow(
        shiny::column(
          3,
          shiny::checkboxInput(ns("batch_mode"), "Batch edit mode", value = FALSE)
        ),
        shiny::column(
          3,
          shiny::uiOutput(ns("commit_button_ui"))
        ),
        shiny::column(
          6,
          shiny::uiOutput(ns("status_text")),
          style = "text-align: right; padding-top: 7px;"
        )
      )
    ),

    DT::DTOutput(ns("table")),

    shiny::div(id = ns("busy"), "Processing cell links...")
  )
}

#' LinkedCells Server Module
#'
#' @export
linked_cells_server <- function(id, data, num_locked_rows = 0, link_fn, tolerance = 0.1) {

  # FIX 4: data validation
  if (!is.data.frame(data)) {
    stop("data must be a dataframe")
  }

  if (!is.function(link_fn)) {
    stop("link_fn must be a function")
  }

  shiny::moduleServer(id, function(input, output, session) {

    linked_cells_init()

    state <- shiny::reactiveValues(
      committed = data,
      in_editor = data,
      is_computing = FALSE,
      exec_token = 0,
      active_token = 0,
      last_status = "ready",
      last_message = ""
    )

    table_proxy <- DT::dataTableProxy(session$ns("table"), session = session)

    output$commit_button_ui <- shiny::renderUI({
      if (isTRUE(input$batch_mode)) {
        shiny::actionButton(
          session$ns("commit"),
          if (state$is_computing) "Processing..." else "Commit Changes",
          class = if (state$is_computing) "btn-warning btn-sm" else "btn-primary btn-sm",
          disabled = state$is_computing
        )
      }
    })

    output$table <- DT::renderDT({

      display_data <- if (isTRUE(input$batch_mode)) state$in_editor else state$committed

      DT::datatable(
        display_data,
        rownames = FALSE,
        selection = "none",
        editable = list(
          target = "cell",
          disable = list(rows = seq_len(num_locked_rows))
        ),
        options = list(
          dom = "t",
          ordering = FALSE,
          paging = FALSE,
          info = FALSE,
          searching = FALSE
        )
      ) |>
        DT::formatRound(columns = which(vapply(display_data, is.numeric, logical(1))), digits = 2)
    })

    shiny::observeEvent(state$committed, {
      display_data <- if (isTRUE(input$batch_mode)) state$in_editor else state$committed
      DT::replaceData(table_proxy, display_data, resetPaging = FALSE)
    }, ignoreInit = TRUE)

    shiny::observeEvent(state$in_editor, {
      if (isTRUE(input$batch_mode)) {
        DT::replaceData(table_proxy, state$in_editor, resetPaging = FALSE)
      }
    }, ignoreInit = TRUE)

    # -------------------------
    # Cell edit handler
    # -------------------------
    shiny::observeEvent(input$table_cell_edit, {

      info <- input$table_cell_edit
      row_idx <- info$row
      col_idx <- info$col + 1
      new_value <- info$value

      if (row_idx <= num_locked_rows) {
        shiny::showNotification("This row is read-only", type = "warning", duration = 2)
        DT::replaceData(table_proxy, state$committed)
        return()
      }

      if (state$is_computing) {
        shiny::showNotification("Processing. Please wait", type = "message", duration = 2)
        DT::replaceData(
          table_proxy,
          if (isTRUE(input$batch_mode)) state$in_editor else state$committed
        )
        return()
      }

      if (isTRUE(input$batch_mode)) {

        buf <- state$in_editor
        old_val <- buf[row_idx, col_idx]

        buf[row_idx, col_idx] <-
          if (is.numeric(old_val)) as.numeric(new_value) else new_value

        state$in_editor <- buf
        state$last_status <- "buffered"
        state$last_message <- sprintf("Row %d queued", row_idx)

        DT::replaceData(table_proxy, buf)
        return()
      }

      # Immediate mode
      state$is_computing <- TRUE
      shinyjs::addClass(session$ns("busy"), "show")

      data_with_edit <- state$committed
      old_val <- data_with_edit[row_idx, col_idx]

      data_with_edit[row_idx, col_idx] <-
        if (is.numeric(old_val)) as.numeric(new_value) else new_value

      edits <- make_edits_df(row_idx, col_idx)

      exec_token <- generate_token()
      state$active_token <- exec_token

      # FIX 1: replace %...>% with promises::then
      promises::then(
        with_async_safety(
          {
            link_fn(data_with_edit, edits)
          },
          session = session
        ),
        onFulfilled = function(result) {

          if (!is_token_active(exec_token, state$active_token)) {
            state$is_computing <- FALSE
            shinyjs::removeClass(session$ns("busy"), "show")  # FIX 2
            return(NULL)
          }

          if (!is.list(result) || !("data" %in% names(result)) || !("status" %in% names(result))) {
            state$is_computing <- FALSE
            state$last_status <- "error"
            shinyjs::removeClass(session$ns("busy"), "show")  # FIX 2
            shiny::showNotification("Invalid link_fn result", type = "error")
            DT::replaceData(table_proxy, state$committed)
            return(NULL)
          }

          state$last_status <- result$status
          state$last_message <- result$message %||% ""

          switch(result$status,
                 "success" = {
                   state$committed <- result$data
                   state$in_editor <- result$data
                 },
                 "invalid" = {
                   state$in_editor <- state$committed
                 },
                 "not_possible" = {
                   state$in_editor <- state$committed
                 },
                 "unchanged" = {
                   state$in_editor <- state$committed
                 }
          )

          state$is_computing <- FALSE
          shinyjs::removeClass(session$ns("busy"), "show")
        }
      )
    })

    # -------------------------
    # Commit handler
    # -------------------------
    shiny::observeEvent(input$commit, {

      if (!isTRUE(input$batch_mode) || state$is_computing) return()

      changes <- detect_changes(
        state$committed,
        state$in_editor,
        num_locked_rows,
        tolerance
      )

      if (nrow(changes) == 0) {
        shiny::showNotification("No changes", type = "message", duration = 2)
        return()
      }

      state$is_computing <- TRUE
      shinyjs::addClass(session$ns("busy"), "show")

      exec_token <- generate_token()
      state$active_token <- exec_token

      # FIX 1
      promises::then(
        with_async_safety(
          {
            link_fn(state$in_editor, changes)
          },
          session = session
        ),
        onFulfilled = function(result) {

          if (!is_token_active(exec_token, state$active_token)) {
            state$is_computing <- FALSE
            shinyjs::removeClass(session$ns("busy"), "show")  # FIX 2
            return(NULL)
          }

          # FIX 3: enforce full contract
          if (!is.list(result) || !("data" %in% names(result)) || !("status" %in% names(result))) {
            state$is_computing <- FALSE
            shinyjs::removeClass(session$ns("busy"), "show")  # FIX 2
            shiny::showNotification("Invalid link_fn result", type = "error")
            DT::replaceData(table_proxy, state$committed)
            return(NULL)
          }

          state$last_status <- result$status

          if (result$status == "success") {
            state$committed <- result$data
            state$in_editor <- result$data
          } else {
            state$in_editor <- state$committed
          }

          state$is_computing <- FALSE
          shinyjs::removeClass(session$ns("busy"), "show")
        }
      )
    })

    output$status_text <- shiny::renderUI({

      emoji <- switch(
        state$last_status,
        "ready" = "OK",
        "success" = "OK",
        "buffered" = "EDIT",
        "invalid" = "WARN",
        "error" = "ERR",
        "?"
      )

      color <- switch(
        state$last_status,
        "success" = "#4CAF50",
        "buffered" = "#2196F3",
        "invalid" = "#FF9800",
        "error" = "#f44336",
        "#666"
      )

      msg <-
        if (nchar(state$last_message) > 0) {
          sprintf("%s %s (%s)", emoji, state$last_status, state$last_message)
        } else {
          sprintf("%s %s", emoji, state$last_status)
        }

      shiny::HTML(sprintf(
        '<span style="color: %s; font-size: 0.9em;">%s</span>',
        color,
        msg
      ))
    })

    shiny::reactive(state$committed)
  })
}

`%||%` <- function(x, y) {
  if (is.null(x) || is.na(x)) y else x
}
