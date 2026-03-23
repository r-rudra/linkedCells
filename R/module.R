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

        /* -- Controls bar -- */
        #%s {
          padding: 12px;
          background: #f5f5f5;
          border-bottom: 1px solid #ddd;
          margin-bottom: 12px;
          border-radius: 4px;
        }

        /* -- Flash animation: yellow pulse then fade (3.5s) -- */
        @keyframes lc-flash {
          0%%   { background-color: #FFF176; }
          30%%  { background-color: #FFEE58; }
          100%% { background-color: transparent; }
        }
        #%s .linked-flash {
          animation: lc-flash 3.5s ease-out forwards !important;
        }

        /* -- Vertical borders -- */
        #%s table.dataTable td,
        #%s table.dataTable th {
          border-right: 1px solid #cce0f0;
        }

        #%s table.dataTable {
          border-left:  1px solid #cce0f0;
          border-right: 1px solid #cce0f0;
        }

        /* -- Header: light blue theme -- */
        #%s table.dataTable thead th {
          background-color: #D6EAF8 !important;
          color: #1A5276 !important;
          font-weight: 700;
          border-bottom: 2px solid #AED6F1 !important;
          border-right:  1px solid #AED6F1 !important;
        }

        /* -- Locked rows: greyed out, blocked cursor -- */
        #%s table.dataTable tbody tr.lc-locked td {
          background-color: #F4F6F7 !important;
          color: #7F8C8D !important;
          cursor: not-allowed !important;
          font-style: italic;
        }

      ",
                                            ns("controls"),   # 1  controls bar
                                            ns("table"),      # 2  flash animation
                                            ns("table"),      # 3  td border
                                            ns("table"),      # 4  th border
                                            ns("table"),      # 5  outer border
                                            ns("table"),      # 6  thead blue
                                            ns("table")       # 7  locked rows
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

    DT::DTOutput(ns("table"))
  )
}


#' LinkedCells Server Module
#'
#' @param id Module namespace id
#' @param data Initial data.frame
#' @param num_locked_rows Number of read-only rows at the top (default 0)
#' @param link_fn Function(data, edits) returning list(data, status, message)
#' @param tolerance Numeric tolerance for change detection (default 0.1)
#'
#' @return A reactive returning the current committed data.frame
#'
#' @export
linked_cells_server <- function(id, data, num_locked_rows = 0, link_fn, tolerance = 0.1) {

  if (!is.data.frame(data)) stop("data must be a dataframe")
  if (!is.function(link_fn)) stop("link_fn must be a function")

  shiny::moduleServer(id, function(input, output, session) {

    linked_cells_init()

    # ---- Reactive state ----
    state <- shiny::reactiveValues(
      committed    = data,
      in_editor    = data,
      is_computing = FALSE,
      active_token = 0,
      last_status  = "ready",
      last_message = ""
    )

    table_proxy  <- DT::dataTableProxy(session$ns("table"), session = session)
    dt_dom_id    <- session$ns("table")
    dt_dom_id_js <- jsonlite::toJSON(dt_dom_id, auto_unbox = TRUE)
    busy_id      <- session$ns("lc_busy")

    # ================================================================
    # Helpers
    # ================================================================

    show_busy <- function() {
      shiny::showNotification(
        shiny::tags$span(
          shiny::tags$strong("Processing cell links..."),
          style = "font-size: 14px;"
        ),
        id       = busy_id,
        duration = NULL,
        type     = "message",
        session  = session
      )
    }

    hide_busy <- function() {
      shiny::removeNotification(id = busy_id, session = session)
    }

    notify <- function(msg, type = "message", duration = 3) {
      shiny::showNotification(msg, type = type, duration = duration, session = session)
    }

    # Build 0-based cell list for JS from two dataframes
    build_flash_cells <- function(old_df, new_df) {
      ch <- detect_changes(old_df, new_df, num_locked_rows, tolerance)
      if (nrow(ch) == 0L) return(list())
      lapply(seq_len(nrow(ch)), function(i) {
        list(row = as.integer(ch$row[i]) - 1L,
             col = as.integer(ch$col[i]) - 1L)
      })
    }

    # Inject JS to add linked-flash class on specified cells
    fire_flash <- function(cells_0based) {
      if (length(cells_0based) == 0L) return(invisible(NULL))
      cells_json <- jsonlite::toJSON(cells_0based, auto_unbox = TRUE)
      shinyjs::runjs(sprintf("
        (function() {
          var domId = %s;
          var cells = %s;
          setTimeout(function() {
            var api = window._lcApi && window._lcApi[domId];
            if (!api) return;
            cells.forEach(function(c) {
              var node = api.row(c.row).node();
              if (!node) return;
              var td = $('td:eq(' + c.col + ')', node);
              if (!td.length) return;
              td.removeClass('linked-flash');
              void td[0].offsetWidth;
              td.addClass('linked-flash');
            });
          }, 60);
        })();
      ", dt_dom_id_js, cells_json))
    }

    # Replace table data and optionally flash changed cells
    update_table <- function(display_data, flash_cells = list()) {
      DT::replaceData(table_proxy, display_data, resetPaging = FALSE, rownames = FALSE)
      if (length(flash_cells) > 0L) fire_flash(flash_cells)
    }

    # Process link_fn result inside a promise callback
    handle_result <- function(result, exec_token, committed_snapshot) {

      # Stale execution guard
      if (!is_token_active(exec_token, state$active_token)) {
        state$is_computing <- FALSE
        hide_busy()
        return(NULL)
      }

      # Shape validation
      if (!is.list(result) ||
          !("data"   %in% names(result)) ||
          !("status" %in% names(result))) {
        state$is_computing <- FALSE
        state$last_status  <- "error"
        hide_busy()
        notify("link_fn returned an invalid result.", type = "error", duration = 5)
        update_table(state$committed)
        return(NULL)
      }

      state$last_status  <- result$status
      state$last_message <- result$message %||% ""

      switch(result$status,

             "success" = {
               flash <- build_flash_cells(committed_snapshot, result$data)
               state$committed <- result$data
               state$in_editor <- result$data
               update_table(result$data, flash)
               msg <- result$message %||% ""
               if (nzchar(msg)) notify(msg, duration = 2)
             },

             "invalid" = {
               state$in_editor <- state$committed
               update_table(state$committed)
               notify(result$message %||% "Invalid value.", type = "warning", duration = 4)
             },

             "not_possible" = {
               state$in_editor <- state$committed
               update_table(state$committed)
               notify(result$message %||% "Change not possible.", type = "warning", duration = 4)
             },

             "unchanged" = {
               state$in_editor <- state$committed
               update_table(state$committed)
             }
      )

      state$is_computing <- FALSE
      hide_busy()
    }

    # ================================================================
    # Commit button UI (batch mode only)
    # ================================================================

    output$commit_button_ui <- shiny::renderUI({
      if (isTRUE(input$batch_mode)) {
        shiny::actionButton(
          session$ns("commit"),
          if (state$is_computing) "Processing..." else "Commit Changes",
          class    = if (state$is_computing) "btn-warning btn-sm" else "btn-primary btn-sm",
          disabled = state$is_computing
        )
      }
    })

    # ================================================================
    # Initial table render
    # ================================================================

    output$table <- DT::renderDT({

      display_data <- if (isTRUE(input$batch_mode)) state$in_editor else state$committed

      editable_cfg <- list(target = "cell")
      if (num_locked_rows > 0L) {
        editable_cfg$disable <- list(rows = seq_len(num_locked_rows))
      }

      # initComplete JS:
      #   - stores DataTables api on window._lcApi[domId] for flash calls
      #   - marks locked rows with lc-locked class (re-applies on draw)
      #   - hard-blocks dblclick/mousedown on locked rows
      init_js <- DT::JS(sprintf("
        function(settings, json) {
          var api    = this.api();
          var locked = %d;
          var domId  = %s;

          if (!window._lcApi) window._lcApi = {};
          window._lcApi[domId] = api;

          function markLocked() {
            if (locked <= 0) return;
            api.rows().every(function(rIdx) {
              if (rIdx < locked) {
                $(this.node()).addClass('lc-locked');
              }
            });
          }

          markLocked();
          api.on('draw.dt', function() { markLocked(); });

          $(api.table().node()).on('dblclick mousedown', 'td', function(e) {
            var idx = api.cell(this).index();
            if (idx && idx.row < locked) {
              e.stopImmediatePropagation();
              e.preventDefault();
              return false;
            }
          });
        }
      ", num_locked_rows, dt_dom_id_js))

      DT::datatable(
        display_data,
        rownames  = FALSE,
        selection = "none",
        class     = "table table-striped table-hover table-bordered",
        editable  = editable_cfg,
        options   = list(
          dom          = "t",
          ordering     = FALSE,
          paging       = FALSE,
          info         = FALSE,
          searching    = FALSE,
          initComplete = init_js
        )
      ) |>
        DT::formatRound(
          columns = which(vapply(display_data, is.numeric, logical(1))),
          digits  = 2
        )
    })

    # ================================================================
    # Cell edit handler
    # ================================================================

    shiny::observeEvent(input$table_cell_edit, {

      info      <- input$table_cell_edit
      row_idx   <- info$row
      col_idx   <- info$col + 1L
      new_value <- info$value

      # Guard: locked row
      if (row_idx <= num_locked_rows) {
        update_table(state$committed)
        return()
      }

      # Guard: already computing
      if (state$is_computing) {
        notify("Still processing previous edit, please wait.", type = "warning")
        update_table(
          if (isTRUE(input$batch_mode)) state$in_editor else state$committed
        )
        return()
      }

      # ---- Batch mode: buffer only, no link_fn yet ----
      if (isTRUE(input$batch_mode)) {
        buf     <- state$in_editor
        old_val <- buf[row_idx, col_idx]
        buf[row_idx, col_idx] <-
          if (is.numeric(old_val)) as.numeric(new_value) else new_value
        state$in_editor    <- buf
        state$last_status  <- "buffered"
        state$last_message <- sprintf("Row %d queued", row_idx)
        update_table(buf)
        return()
      }

      # ---- Immediate mode: run link_fn async ----
      state$is_computing <- TRUE
      show_busy()

      data_with_edit <- state$committed
      old_val        <- data_with_edit[row_idx, col_idx]
      data_with_edit[row_idx, col_idx] <-
        if (is.numeric(old_val)) as.numeric(new_value) else new_value

      edits              <- make_edits_df(row_idx, col_idx)
      exec_token         <- generate_token()
      state$active_token <- exec_token
      committed_snapshot <- state$committed

      promises::then(
        with_async_safety(
          { link_fn(data_with_edit, edits) },
          session = session
        ),
        onFulfilled = function(result) {
          handle_result(result, exec_token, committed_snapshot)
        }
      )
    })

    # ================================================================
    # Batch commit handler
    # ================================================================

    shiny::observeEvent(input$commit, {
      if (!isTRUE(input$batch_mode)) return()

      if (state$is_computing) {
        notify("Already processing, please wait.", type = "warning")
        return()
      }

      if (identical(state$in_editor, state$committed)) {
        notify("No changes to commit.", type = "message")
        return()
      }

      state$is_computing <- TRUE
      show_busy()

      changes <- detect_changes(state$committed, state$in_editor, num_locked_rows, tolerance)
      edits <- if (nrow(changes) > 0L) {
        data.frame(
          row = changes$row,
          col = changes$col,
          stringsAsFactors = FALSE
        )
      } else {
        make_edits_df(integer(0), integer(0))
      }

      data_to_send       <- state$in_editor
      exec_token         <- generate_token()
      state$active_token <- exec_token
      committed_snapshot <- state$committed

      promises::then(
        with_async_safety(
          { link_fn(data_to_send, edits) },
          session = session
        ),
        onFulfilled = function(result) {
          handle_result(result, exec_token, committed_snapshot)
        }
      )
    })

    # ================================================================
    # Return reactive committed data
    # ================================================================

    shiny::reactive(state$committed)
  })
}


`%||%` <- function(x, y) {
  if (is.null(x) || (length(x) == 1L && is.na(x))) y else x
}
