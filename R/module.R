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
        #%s {
          padding: 12px;
          background: #f5f5f5;
          border-bottom: 1px solid #ddd;
          margin-bottom: 12px;
          border-radius: 4px;
        }

        #%s .linked-edited {
          background-color: #FFEB3B !important;
          font-weight: bold;
        }

        #%s .linked-adjusted {
          background-color: #FFE5B4 !important;
        }

        /* Vertical borders */
        #%s table.dataTable td,
        #%s table.dataTable th {
          border-right: 1px solid #ddd;
        }

        #%s table.dataTable {
          border-left: 1px solid #ddd;
          border-right: 1px solid #ddd;
        }

        /* Header styling */
        #%s thead th {
          background-color: #f8f9fa;
          font-weight: 600;
        }

        #%s {
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

        #%s.show {
          display: block;
        }
      ",
                                            ns("controls"),  # 1: controls div
                                            ns("table"),     # 2: .linked-edited
                                            ns("table"),     # 3: .linked-adjusted
                                            ns("table"),     # 4: td/th border-right
                                            ns("table"),     # 5: td/th border-right (th)
                                            ns("table"),     # 6: table border
                                            ns("table"),     # 7: thead th
                                            ns("busy"),      # 8: busy display:none
                                            ns("busy")       # 9: busy.show
      )))
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

      changes <- detect_changes(
        state$committed,
        display_data,
        num_locked_rows,
        tolerance
      )

      # Build JS array of changed cells (0-indexed for DataTables)
      changed_js <- if (nrow(changes) > 0) {
        paste(
          apply(changes, 1, function(x) {
            sprintf("{row:%d,col:%d}", as.integer(x["row"]) - 1L, as.integer(x["col"]) - 1L)
          }),
          collapse = ","
        )
      } else {
        ""
      }

      # Editable config: disable locked rows (1-based for DT)
      editable_cfg <- list(target = "cell")
      if (num_locked_rows > 0L) {
        editable_cfg$disable <- list(rows = seq_len(num_locked_rows))
      }

      # initComplete runs after table is fully drawn — correct hook for row/cell styling
      # callback (top-level DT arg) is for column defs only; post-draw logic must use initComplete
      init_js <- DT::JS(sprintf("
        function(settings, json) {
          var table = this.api();
          var changes = [%s];
          var locked  = %d;

          // Highlight edited cells
          if (changes.length > 0) {
            table.rows().every(function(rowIdx) {
              var rowNode = this.node();
              changes.forEach(function(cell) {
                if (cell.row === rowIdx) {
                  $('td:eq(' + cell.col + ')', rowNode).addClass('linked-edited');
                }
              });
            });
          }

          // Block clicks on locked rows
          if (locked > 0) {
            $(table.table().node()).on('click', 'td', function() {
              var cellInfo = table.cell(this).index();
              if (cellInfo && cellInfo.row < locked) {
                return false;
              }
            });
          }
        }
      ", changed_js, num_locked_rows))

      DT::datatable(
        display_data,
        rownames    = FALSE,
        selection   = "none",
        class       = "table table-striped table-hover table-bordered",
        editable    = editable_cfg,
        options     = list(
          dom       = "t",
          ordering  = FALSE,
          paging    = FALSE,
          info      = FALSE,
          searching = FALSE,
          initComplete = init_js
        )
      ) |>
        DT::formatRound(
          columns = which(vapply(display_data, is.numeric, logical(1))),
          digits  = 2
        )
    })

    # Replace data on committed change (avoids full re-render)
    shiny::observeEvent(state$committed, {
      display_data <- if (isTRUE(input$batch_mode)) state$in_editor else state$committed
      DT::replaceData(table_proxy, display_data, resetPaging = FALSE, rownames = FALSE)
    }, ignoreInit = TRUE)

    # Replace data when editor buffer changes in batch mode
    shiny::observeEvent(state$in_editor, {
      if (isTRUE(input$batch_mode)) {
        DT::replaceData(table_proxy, state$in_editor, resetPaging = FALSE, rownames = FALSE)
      }
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$table_cell_edit, {

      info      <- input$table_cell_edit
      row_idx   <- info$row          # 1-based from DT
      col_idx   <- info$col + 1L     # DT sends 0-based col; +1 for R indexing
      new_value <- info$value

      # Guard: locked row
      if (row_idx <= num_locked_rows) {
        shiny::showNotification("This row is read-only", type = "warning", duration = 2)
        DT::replaceData(table_proxy, state$committed, resetPaging = FALSE, rownames = FALSE)
        return()
      }

      # Guard: already computing
      if (state$is_computing) {
        shiny::showNotification("Processing. Please wait", type = "message", duration = 2)
        DT::replaceData(
          table_proxy,
          if (isTRUE(input$batch_mode)) state$in_editor else state$committed,
          resetPaging = FALSE, rownames = FALSE
        )
        return()
      }

      # ── Batch mode: buffer the edit, don't run link_fn yet ──
      if (isTRUE(input$batch_mode)) {

        buf     <- state$in_editor
        old_val <- buf[row_idx, col_idx]

        buf[row_idx, col_idx] <-
          if (is.numeric(old_val)) as.numeric(new_value) else new_value

        state$in_editor   <- buf
        state$last_status  <- "buffered"
        state$last_message <- sprintf("Row %d queued", row_idx)

        DT::replaceData(table_proxy, buf, resetPaging = FALSE, rownames = FALSE)
        return()
      }

      # ── Immediate mode: run link_fn async ──
      state$is_computing <- TRUE
      shinyjs::addClass(session$ns("busy"), "show")

      data_with_edit <- state$committed
      old_val        <- data_with_edit[row_idx, col_idx]

      data_with_edit[row_idx, col_idx] <-
        if (is.numeric(old_val)) as.numeric(new_value) else new_value

      edits       <- make_edits_df(row_idx, col_idx)
      exec_token  <- generate_token()
      state$active_token <- exec_token

      promises::then(
        with_async_safety(
          {
            link_fn(data_with_edit, edits)
          },
          session = session
        ),
        onFulfilled = function(result) {

          # Stale execution guard
          if (!is_token_active(exec_token, state$active_token)) {
            state$is_computing <- FALSE
            shinyjs::removeClass(session$ns("busy"), "show")
            return(NULL)
          }

          # Validate result shape
          if (!is.list(result) ||
              !("data"   %in% names(result)) ||
              !("status" %in% names(result))) {
            state$is_computing <- FALSE
            state$last_status  <- "error"
            shinyjs::removeClass(session$ns("busy"), "show")
            shiny::showNotification("Invalid link_fn result", type = "error")
            DT::replaceData(table_proxy, state$committed, resetPaging = FALSE, rownames = FALSE)
            return(NULL)
          }

          state$last_status  <- result$status
          state$last_message <- result$message %||% ""

          switch(result$status,
                 "success" = {
                   state$committed <- result$data
                   state$in_editor <- result$data
                 },
                 "invalid" = {
                   state$in_editor <- state$committed
                   DT::replaceData(table_proxy, state$committed, resetPaging = FALSE, rownames = FALSE)
                 },
                 "not_possible" = {
                   state$in_editor <- state$committed
                   DT::replaceData(table_proxy, state$committed, resetPaging = FALSE, rownames = FALSE)
                 },
                 "unchanged" = {
                   state$in_editor <- state$committed
                   DT::replaceData(table_proxy, state$committed, resetPaging = FALSE, rownames = FALSE)
                 }
          )

          state$is_computing <- FALSE
          shinyjs::removeClass(session$ns("busy"), "show")
        }
      )
    })

    shiny::reactive(state$committed)
  })
}

`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}
