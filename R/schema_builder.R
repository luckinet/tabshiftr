#' Interactively build a schema description
#'
#' Opens a Shiny app in your browser that guides you through a visual decision
#' tree to describe the arrangement of your table, then generates the
#' corresponding \code{schema} R code.
#'
#' @param input [\code{data.frame(1)}]\cr Optionally, pass in your table
#'   directly. If \code{NULL} (default), the app shows a file upload widget.
#' @details Requires the \pkg{shiny} and \pkg{DT} packages. Install them with
#'   \code{install.packages(c("shiny", "DT"))}.
#' @return Invisibly returns the evaluated \code{schema} object when the user
#'   clicks \emph{Finish}, or \code{NULL} if the app is closed without finishing.
#' @examples
#' \dontrun{
#' schema_builder()
#' schema_builder(input = tabs2shift$clusters_horizontal)
#' }
#' @importFrom utils read.table count.fields
#' @export

schema_builder <- function(input = NULL) {

  if (!requireNamespace("shiny", quietly = TRUE))
    stop("Package 'shiny' is required: install.packages('shiny')")
  if (!requireNamespace("DT", quietly = TRUE))
    stop("Package 'DT' is required: install.packages('DT')")

  options(`tabshiftr.builder_input` = input)
  result_env <- new.env(parent = emptyenv())
  result_env$schema <- NULL

  shiny::runApp(.sb_build_app(result_env), launch.browser = TRUE)
  return(result_env$schema)
}

# ---------------------------------------------------------------------------
# Colour palette -- shared across ui and server
# ---------------------------------------------------------------------------

.SB <- list(
  id      = "#5B9BD5",   # blue -- id values
  id_key  = "#2E6DAD",   # darker blue -- id name cell
  obs     = "#ED7D31",   # orange -- obs values
  obs_key = "#C45A10",   # darker orange -- obs name cell
  cluster = "#70AD47",   # green
  empty   = "#F2F2F2",   # light grey -- empty/ignored cell
  header  = "#D9D9D9",   # mid grey -- header row
  none    = "#FFFFFF"    # white -- unassigned data cell
)

# ---------------------------------------------------------------------------
# Thumbnail generator -- pure colour div grids, no text
# colour_matrix: character matrix of .SB colour codes, row = table row
# ---------------------------------------------------------------------------

.sb_thumb <- function(colour_matrix, cell_px = 14, gap_px = 2) {
  nr <- nrow(colour_matrix)
  nc <- ncol(colour_matrix)
  rows_html <- paste(vapply(seq_len(nr), function(r) {
    cells <- paste(vapply(seq_len(nc), function(c) {
      sprintf("<div style='width:%dpx;height:%dpx;background:%s;border-radius:2px;'></div>",
              cell_px, cell_px, colour_matrix[r, c])
    }, character(1)), collapse = "")
    sprintf("<div style='display:flex;gap:%dpx;'>%s</div>", gap_px, cells)
  }, character(1)), collapse = "")
  sprintf("<div style='display:inline-flex;flex-direction:column;gap:%dpx;'>%s</div>",
          gap_px, rows_html)
}

# Pre-built thumbnails for each table type
.sb_thumbs <- local({
  B <- .SB$id; O <- .SB$obs; H <- .SB$header; E <- .SB$empty; N <- .SB$none
  G <- .SB$cluster

  list(
    # Tidy: id cols (blue) + obs cols (orange), single header row (grey)
    tidy = .sb_thumb(matrix(c(
      H, H, H, H,
      B, B, O, O,
      B, B, O, O,
      B, B, O, O
    ), nrow = 4, byrow = TRUE)),

    # Listed: key col (blue) + value col (orange), single header, key repeats
    listed = .sb_thumb(matrix(c(
      H, H,
      B, O,
      B, O,
      B, O,
      B, O,
      B, O,
      B, O
    ), nrow = 7, byrow = TRUE)),

    # Wide ID: two header rows -- ID values in row1 (blue), obs names in row2 (orange)
    wide_id = .sb_thumb(matrix(c(
      H, B, B, B, B,
      H, O, O, O, O,
      B, N, N, N, N,
      B, N, N, N, N,
      B, N, N, N, N
    ), nrow = 5, byrow = TRUE)),

    # Wide ID + listed: two header rows + key-value pair
    wide_listed = .sb_thumb(matrix(c(
      H, B, B, H,
      H, O, O, H,
      B, N, N, B,
      B, N, N, B,
      B, N, N, B
    ), nrow = 5, byrow = TRUE)),

    # Horizontal clusters: two blocks side by side (green divider)
    horiz = .sb_thumb(matrix(c(
      G, H, H, E, G, H, H,
      G, B, O, E, G, B, O,
      G, B, O, E, G, B, O,
      G, B, O, E, G, B, O
    ), nrow = 4, byrow = TRUE)),

    # Vertical clusters: two blocks stacked (green divider row)
    vert = .sb_thumb(matrix(c(
      G, H, H,
      G, B, O,
      G, B, O,
      E, E, E,
      G, H, H,
      G, B, O,
      G, B, O
    ), nrow = 7, byrow = TRUE)),

    # Implicit cluster: cluster header in green, ID value injected (shown as empty)
    implicit = .sb_thumb(matrix(c(
      G, G, G,
      H, H, H,
      B, O, O,
      B, O, O,
      B, O, O
    ), nrow = 5, byrow = TRUE)),

    # Clusters of observed vars: two vertical blocks, each a diff obs var
    obs_clusters = .sb_thumb(matrix(c(
      G, B, B, O,
      G, B, B, O,
      G, B, B, O,
      E, E, E, E,
      G, B, B, O,
      G, B, B, O,
      G, B, B, O
    ), nrow = 7, byrow = TRUE)),

    # Nested clusters: group header (green wide) over sub-clusters
    nested = .sb_thumb(matrix(c(
      G, G, G, G,
      G, H, H, H,
      G, B, B, O,
      G, B, B, O,
      G, G, G, G,
      G, H, H, H,
      G, B, B, O
    ), nrow = 7, byrow = TRUE)),

    # Messy: irregular block positions (shown as offset blocks)
    messy = .sb_thumb(matrix(c(
      G, O, O, E, E,
      G, O, O, E, E,
      E, E, E, G, O,
      E, E, E, G, O,
      E, E, E, G, O,
      G, O, O, E, E
    ), nrow = 6, byrow = TRUE)),

    # Single block yes/no thumbnail
    one_block = .sb_thumb(matrix(c(
      H, H, H, H,
      N, N, N, N,
      N, N, N, N,
      N, N, N, N
    ), nrow = 4, byrow = TRUE)),

    multi_block = .sb_thumb(matrix(c(
      H, H, E, H, H,
      N, N, E, N, N,
      N, N, E, N, N,
      N, N, E, N, N
    ), nrow = 4, byrow = TRUE))
  )
})

# ---------------------------------------------------------------------------
# Choice card HTML -- thumb + label, no body text
# ---------------------------------------------------------------------------

.sb_card <- function(group, value, thumb_html, label) {
  paste0(
    "<div class='sb-card' data-group='", group, "' data-value='", value, "'>",
    thumb_html,
    "<div class='sb-card-label'>", label, "</div>",
    "</div>"
  )
}

# ---------------------------------------------------------------------------
# Clickable table grid rendered from a data.frame
# ---------------------------------------------------------------------------

.sb_grid <- function(tbl, highlight = list(), header_rows = 0L, id = "sb_grid") {
  nc <- ncol(tbl); nr <- nrow(tbl)

  cell_bg <- function(r, c) {
    color <- if (r <= header_rows) .SB$header else .SB$none
    for (h in highlight)
      if (r %in% h$row && c %in% h$col) color <- h$color
    color
  }

  col_heads <- paste(vapply(seq_len(nc), function(c)
    sprintf("<th data-row='0' data-col='%d'>%d</th>", c, c),
    character(1)), collapse = "")

  body <- paste(vapply(seq_len(nr), function(r) {
    cells <- paste(vapply(seq_len(nc), function(c) {
      val <- tbl[[c]][[r]]; val <- if (is.null(val) || is.na(val)) "" else as.character(val)
      sprintf("<td data-row='%d' data-col='%d' style='background:%s;'>%s</td>",
              r, c, cell_bg(r, c), val)
    }, character(1)), collapse = "")
    paste0("<tr>", cells, "</tr>")
  }, character(1)), collapse = "")

  shiny::HTML(sprintf(
    "<div id='%s'><table><thead><tr>%s</tr></thead><tbody>%s</tbody></table></div>",
    id, col_heads, body))
}

# ---------------------------------------------------------------------------
# Build the app
# ---------------------------------------------------------------------------

.sb_build_app <- function(result_env) {

  # ---- CSS ----------------------------------------------------------------
  css <- "
    * { box-sizing: border-box; }
    body { font-family: 'Segoe UI', sans-serif; font-size: 14px;
           background: #f5f5f5; margin: 0; }

    /* two-column layout */
    #sb-outer { display: flex; gap: 16px; padding: 16px;
                max-width: 1200px; margin: auto; align-items: flex-start; }
    #sb-left  { flex: 1 1 60%; min-width: 0; }
    #sb-right { flex: 0 0 360px; position: sticky; top: 16px;
                display: flex; flex-direction: column; gap: 12px; }

    /* panels */
    .sb-panel { background: #fff; border: 1px solid #ddd;
                border-radius: 8px; padding: 16px; }
    .sb-panel h3 { margin: 0 0 10px; font-size: 15px; color: #333; }
    .sb-panel h4 { margin: 0 0 6px; font-size: 13px; color: #555;
                   text-transform: uppercase; letter-spacing: .05em; }

    /* phase badge */
    .sb-phase { display: inline-block; font-size: 11px; font-weight: 600;
                padding: 2px 8px; border-radius: 10px; margin-bottom: 8px; }
    .sb-phase-1 { background: #e8f0fb; color: #2a5ca8; }
    .sb-phase-2 { background: #fef0e6; color: #a0440a; }

    /* question block */
    .sb-q { margin-bottom: 14px; }
    .sb-q-text { font-weight: 600; margin-bottom: 8px; color: #222; }
    .sb-hint { font-size: 12px; color: #888; margin-bottom: 8px;
               border-left: 3px solid #ddd; padding-left: 8px; }

    /* choice cards */
    .sb-cards { display: flex; flex-wrap: wrap; gap: 10px; }
    .sb-card { border: 2px solid #ddd; border-radius: 8px; padding: 10px;
               cursor: pointer; text-align: center; background: #fafafa;
               transition: border-color .15s, background .15s; }
    .sb-card:hover   { border-color: #5B9BD5; background: #f0f6fd; }
    .sb-card.selected { border-color: #5B9BD5; background: #deeaf7; }
    .sb-card-label { margin-top: 6px; font-size: 12px; font-weight: 600;
                     color: #333; }

    /* variable pill buttons */
    .sb-var-btn { display: inline-block; padding: 4px 10px; margin: 3px;
                  border-radius: 12px; font-size: 12px; cursor: pointer;
                  border: 2px solid transparent; font-weight: 600; }
    .sb-var-id  { background: #deeaf7; color: #2a5ca8;
                  border-color: #5B9BD5; }
    .sb-var-obs { background: #fde9d9; color: #a0440a;
                  border-color: #ED7D31; }
    .sb-var-active { box-shadow: 0 0 0 3px #333; }

    /* legend */
    .sb-legend { display: flex; flex-wrap: wrap; gap: 8px;
                 margin-bottom: 8px; }
    .sb-swatch { display: inline-flex; align-items: center;
                 font-size: 11px; gap: 4px; }
    .sb-swatch-box { width: 12px; height: 12px; border-radius: 2px;
                     border: 1px solid #ccc; flex-shrink: 0; }

    /* grid */
    #sb_grid table { border-collapse: collapse; font-size: 12px; width: 100%; }
    #sb_grid td, #sb_grid th { border: 1px solid #ccc; padding: 3px 6px;
      cursor: pointer; min-width: 36px; text-align: center; }
    #sb_grid th { background: #e9ecef; font-weight: normal;
                  font-size: 11px; color: #666; }
    #sb_grid td:hover { outline: 2px solid #333; }

    /* code box */
    .sb-code-wrap { position: relative; }
    .sb-code { background: #1e1e1e; color: #d4d4d4; border-radius: 6px;
               min-height: 80px; overflow-x: auto; }
    .sb-code pre { background: transparent; border: none; margin: 0;
                   padding: 12px; font-size: 12px; white-space: pre;
                   overflow-x: visible; word-wrap: normal;
                   word-break: normal; color: inherit; }
    .sb-copy-btn { position: absolute; top: 6px; right: 6px;
                   background: #3a3a3a; color: #ccc; border: none;
                   border-radius: 4px; padding: 3px 7px; font-size: 14px;
                   cursor: pointer; opacity: 0; transition: opacity .15s; }
    .sb-code-wrap:hover .sb-copy-btn { opacity: 1; }
    .sb-copy-btn:hover { background: #555; color: #fff; }

    /* progress dots */
    .sb-dots { display: flex; gap: 6px; margin-bottom: 12px; }
    .sb-dot { width: 8px; height: 8px; border-radius: 50%;
              background: #ddd; transition: background .2s; }
    .sb-dot.done { background: #5B9BD5; }
    .sb-dot.active { background: #ED7D31; }

    /* action buttons */
    .sb-btn { padding: 7px 16px; border: none; border-radius: 6px;
              cursor: pointer; font-size: 13px; font-weight: 600;
              margin-right: 6px; margin-top: 8px; }
    .sb-btn-primary { background: #5B9BD5; color: #fff; }
    .sb-btn-primary:hover { background: #4a87c0; }
    .sb-btn-danger  { background: #c0392b; color: #fff; }
    .sb-btn-danger:hover  { background: #a93226; }
    .sb-btn-sm { padding: 4px 10px; font-size: 12px;
                 background: #eee; color: #333; border-radius: 4px;
                 border: none; cursor: pointer; }

    /* debug tabs */
    .sb-tabs { display: flex; gap: 0; border-bottom: 2px solid #ddd;
               margin-bottom: 12px; }
    .sb-tab  { padding: 5px 14px; font-size: 12px; font-weight: 600;
               cursor: pointer; color: #666; border: none; background: none;
               border-bottom: 2px solid transparent; margin-bottom: -2px; }
    .sb-tab:hover  { color: #333; }
    .sb-tab.active { color: #2a5ca8; border-bottom-color: #2a5ca8; }
    .sb-tab-panel  { display: none; }
    .sb-tab-panel.active { display: block; }
  "

  # ---- JS -----------------------------------------------------------------
  js <- "
    // cell click
    $(document).on('click', '#sb_grid td', function() {
      Shiny.setInputValue('sb_cell',
        {row: $(this).data('row'), col: $(this).data('col')},
        {priority: 'event'});
    });
    // card selection
    $(document).on('click', '.sb-card', function() {
      var g = $(this).data('group'), v = $(this).data('value');
      $('.sb-card[data-group=\"'+g+'\"]').removeClass('selected');
      $(this).addClass('selected');
      Shiny.setInputValue('sb_card_'+g, v, {priority: 'event'});
    });
    // variable pill click
    $(document).on('click', '.sb-var-btn', function() {
      Shiny.setInputValue('sb_var_click',
        {idx: $(this).data('idx'), type: $(this).data('type')},
        {priority: 'event'});
    });
    // variable pill drag-to-reorder
    var sbDragIdx = null;
    $(document).on('dragstart', '.sb-var-btn', function(e) {
      sbDragIdx = $(this).data('idx');
      e.originalEvent.dataTransfer.effectAllowed = 'move';
    });
    $(document).on('dragover', '.sb-var-btn', function(e) {
      e.preventDefault();
      e.originalEvent.dataTransfer.dropEffect = 'move';
    });
    $(document).on('drop', '.sb-var-btn', function(e) {
      e.preventDefault();
      var toIdx = $(this).data('idx');
      if (sbDragIdx !== null && sbDragIdx !== toIdx) {
        Shiny.setInputValue('sb_var_reorder',
          {from: sbDragIdx, to: toIdx}, {priority: 'event'});
      }
      sbDragIdx = null;
    });
    // debug tabs
    $(document).on('click', '.sb-tab', function() {
      var grp = $(this).data('tabgroup');
      $('.sb-tab[data-tabgroup=\"'+grp+'\"]').removeClass('active');
      $(this).addClass('active');
      $('.sb-tab-panel[data-tabgroup=\"'+grp+'\"]').removeClass('active');
      $('.sb-tab-panel[data-tabgroup=\"'+grp+'\"][data-tabid=\"'+$(this).data('tabid')+'\"]').addClass('active');
    });
  "

  # ---- UI -----------------------------------------------------------------
  ui <- shiny::fluidPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(css)),
      shiny::tags$script(shiny::HTML(js))
    ),
    shiny::tags$div(id = "sb-outer",

      # LEFT: guided questions
      shiny::tags$div(id = "sb-left",

        shiny::tags$div(class = "sb-panel",
          shiny::tags$h3("tabshiftr \u2014 Schema Builder"),
          shiny::uiOutput("sb_dots")
        ),

        # Phase 1: setup (format, row groups, filter)
        shiny::uiOutput("sb_phase1"),

        # Phase 2: variable interview
        shiny::uiOutput("sb_phase2")
      ),

      # RIGHT: sticky table + schema
      shiny::tags$div(id = "sb-right",

        shiny::tags$div(class = "sb-panel",
          shiny::tags$h4("Your table"),
          shiny::uiOutput("sb_legend"),
          shiny::conditionalPanel(
            "output.sb_has_table === 'yes'",
            shiny::div(style = "overflow-x:auto; max-height:320px; overflow-y:auto;",
              shiny::uiOutput("sb_grid_out"))
          ),
          shiny::conditionalPanel(
            "output.sb_has_table === 'no'",
            shiny::fileInput("sb_file", NULL,
              accept = c(".csv", ".tsv", ".txt"),
              buttonLabel = "Browse\u2026",
              placeholder = "CSV / TSV")
          )
        ),

        shiny::tags$div(class = "sb-panel",
          shiny::tags$h4("Schema code"),
          shiny::tags$div(class = "sb-code-wrap",
            shiny::tags$div(class = "sb-code",
              shiny::verbatimTextOutput("sb_code", placeholder = TRUE)),
            shiny::tags$button(
              class   = "sb-copy-btn",
              title   = "Copy to clipboard",
              onclick = "
                var txt = document.getElementById('sb_code').innerText;
                navigator.clipboard.writeText(txt).then(function() {
                  var btn = event.currentTarget;
                  btn.textContent = '\u2713';
                  setTimeout(function() { btn.textContent = '\u29c9'; }, 1500);
                });
              ",
              "\u29c9")
          ),
          shiny::tags$div(style = "display:flex;gap:8px;flex-wrap:wrap;margin-top:10px;",
            shiny::downloadButton("sb_dl", "Download schema as .rds",
              style = "font-size:12px;"),
            shiny::tags$button(class = "sb-btn sb-btn-primary",
              style = "font-size:12px;padding:5px 12px;",
              onclick = "Shiny.setInputValue('sb_reorganise', Math.random(), {priority:'event'})",
              "Reorganise"),
            shiny::tags$button(class = "sb-btn sb-btn-primary",
              style = "font-size:12px;padding:5px 12px;background:#2d6a16;",
              onclick = "Shiny.setInputValue('sb_finish', Math.random(), {priority:'event'})",
              "Finish \u2714")
          )
        ),

        shiny::uiOutput("sb_reorganise_out")
      )
    )
  )

  # ---- SERVER -------------------------------------------------------------
  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(
      tbl          = getOption("tabshiftr.builder_input"),
      phase        = 1L,
      table_type   = "tidy",
      # Phase 1 -- row groups (setGroups / .sum)
      # groups: list of lists, each: list(rows, fill, char_fn, num_fn)
      groups       = list(),
      active_group = NULL,   # index of group currently accepting row clicks
      # Phase 2 -- variable list
      # vars: list of lists, each: list(key, type, step, done, props)
      #   key   = unique string "id_1", "obs_3", etc.
      #   type  = "id" / "obs"
      #   step  = which mini-tree step this var is on (integer)
      #   done  = TRUE once the user clicks the final Done button
      #   props = named list of collected answers
      vars         = list(),
      active_key   = NULL,   # key of var currently being interviewed
      # grid click mode: "col", "row", "origin", "filter_row", "filter_col", "none"
      grid_mode    = "none",
      # Phase 3 -- format (text fields read from input$* at code-gen time, not stored here)
      fmt_flags      = data.frame(flag=character(0), value=character(0),
                                  stringsAsFactors=FALSE),
      # Phase 3 -- filters (list of lists, each: list(mode, rows, cols, invert,
      #             use_find, find_pattern, find_search_in, find_dim, find_select, operator))
      #   find_search_in: "row" or "col" -- which dimension to scan for the pattern
      #   find_dim:       specific row/col number to scan (optional)
      #   find_select:    "rows" or "columns" -- what setFilter argument receives the result
      filters      = list(),
      active_filter = NULL   # index of filter currently being edited
    )

    # -- file upload ---------------------------------------------------------
    shiny::observeEvent(input$sb_file, {
      shiny::req(input$sb_file)
      path <- input$sb_file$datapath
      ext  <- tools::file_ext(input$sb_file$name)
      sep  <- if (ext == "tsv") "\t" else ","
      tbl  <- tryCatch(
        utils::read.table(path, sep = sep, header = FALSE,
          stringsAsFactors = FALSE, fill = TRUE,
          col.names = paste0("V", seq_len(
            max(utils::count.fields(path, sep = sep))))),
        error = function(e) NULL)
      if (!is.null(tbl)) rv$tbl <- tbl
    })


    # helper: has any OTHER obs variable already answered the listed question?
    # count of other obs vars (regardless of done state)
    .n_clusters <- function(vars) {
      for (.v in vars) {
        if (isTRUE(.v$props$clustered == "yes") && length(.v$props$origins %||% list()) > 0)
          return(length(.v$props$origins))
        if (isTRUE(.v$props$cluster_obs == "yes") && length(.v$props$origins %||% list()) > 0)
          return(length(.v$props$origins))
      }
      1L
    }
    .n_other_obs <- function(vars, exclude_key = NULL) {
      sum(vapply(vars, function(x)
        x$type == "obs" && !identical(x$key, exclude_key), logical(1)))
    }
    # TRUE if another obs var has answered the listed question (yes OR no)
    .hla <- function(vars, exclude_key = NULL) {
      any(vapply(vars, function(x)
        x$type == "obs" && !identical(x$key, exclude_key) && !is.null(x$props$listed),
        logical(1)))
    }
    # returns the actual answer ("yes"/"no") from another obs var, or NULL
    .hla_answer <- function(vars, exclude_key = NULL) {
      for (x in vars)
        if (x$type == "obs" && !identical(x$key, exclude_key) && !is.null(x$props$listed))
          return(x$props$listed)
      NULL
    }
    # returns the cluster_obs answer ("yes"/"no") from another obs var, or NULL
    .hco_answer <- function(vars, exclude_key = NULL) {
      for (x in vars)
        if (x$type == "obs" && !identical(x$key, exclude_key) && !is.null(x$props$cluster_obs))
          return(x$props$cluster_obs)
      NULL
    }
    # returns the origins list from whichever obs var first answered cluster_obs == "yes"
    .hco_origins <- function(vars, exclude_key = NULL) {
      for (x in vars)
        if (x$type == "obs" && !identical(x$key, exclude_key) &&
            isTRUE(x$props$cluster_obs == "yes"))
          return(x$props$origins %||% list())
      list()
    }

    # -- cell / col / row clicks in the grid (phase 2) -----------------------
    shiny::observeEvent(input$sb_cell, {
      if (rv$phase != 2L) return()
      r <- input$sb_cell$row
      c <- input$sb_cell$col

      # origin marking -- stored on the active cluster-ID variable
      if (rv$grid_mode == "origin") {
        if (is.null(rv$active_key)) return()
        v    <- rv$vars
        idx  <- which(vapply(v, function(x) x$key == rv$active_key, logical(1)))
        if (length(idx) == 0) return()
        cell_key <- paste0(r, "_", c)
        origins  <- v[[idx]]$props$origins %||% list()
        exists   <- any(vapply(origins, function(o) o$key == cell_key, logical(1)))
        new_origins <- if (exists)
          Filter(function(o) o$key != cell_key, origins)
        else
          c(origins, list(list(row = r, col = c, key = cell_key)))
        # sort by col, then row, so offsets are always computed relative to leftmost block
        new_origins <- new_origins[order(
          vapply(new_origins, `[[`, integer(1), "col"),
          vapply(new_origins, `[[`, integer(1), "row"))]
        v[[idx]]$props$origins <- new_origins
        rv$vars <- v
        return()
      }

      # group origin marking -- stored as group_origins on the active cluster-ID variable
      if (rv$grid_mode == "group_origin") {
        if (is.null(rv$active_key)) return()
        v    <- rv$vars
        idx  <- which(vapply(v, function(x) x$key == rv$active_key, logical(1)))
        if (length(idx) == 0) return()
        cell_key    <- paste0(r, "_", c)
        gorigs      <- v[[idx]]$props$group_origins %||% list()
        exists      <- any(vapply(gorigs, function(o) o$key == cell_key, logical(1)))
        new_gorigs  <- if (exists)
          Filter(function(o) o$key != cell_key, gorigs)
        else
          c(gorigs, list(list(row = r, col = c, key = cell_key)))
        new_gorigs  <- new_gorigs[order(vapply(new_gorigs, `[[`, integer(1), "row"))]
        v[[idx]]$props$group_origins <- new_gorigs
        rv$vars <- v
        return()
      }

      if (is.null(rv$active_key)) return()
      r <- input$sb_cell$row
      c <- input$sb_cell$col
      key  <- rv$active_key
      v    <- rv$vars
      idx  <- which(vapply(v, function(x) x$key == key, logical(1)))
      if (length(idx) == 0) return()
      mode <- rv$grid_mode
      p    <- v[[idx]]$props

      if (mode == "col") {
        # Determine the current step's ui to route the click correctly
        cur_steps <- .sb_steps(v[[idx]]$type, v[[idx]]$props,
                                v[[idx]]$step, .hla(v, v[[idx]]$key), listed_answer = .hla_answer(v, v[[idx]]$key),
                                cluster_obs_answer = .hco_answer(v, v[[idx]]$key),
                                n_other_obs = .n_other_obs(v, v[[idx]]$key), n_clusters = .n_clusters(v))
        cur_ui <- if (v[[idx]]$step <= length(cur_steps)) cur_steps[[v[[idx]]$step]]$ui else ""
        if (v[[idx]]$type == "obs" && cur_ui == "yn_then_kv" &&
            isTRUE(v[[idx]]$props$listed == "yes")) {
          # first click sets key_col; subsequent clicks toggle val_col (vector)
          if (is.null(p$key_col))
            p$key_col <- c
          else {
            vcols <- p$val_col %||% integer(0)
            p$val_col <- if (c %in% vcols) setdiff(vcols, c) else sort(c(vcols, c))
          }
        } else {
          cols <- p$columns %||% integer(0)
          # auto-expand to equivalent columns in all clusters if any clustered var has origins
          # skip for distinct variables -- they appear in only one cluster, not all
          clust_origins <- NULL
          if (!isTRUE(p$distinct == "yes")) {
            for (.cv in v) {
              if (isTRUE(.cv$props$clustered == "yes") && length(.cv$props$origins %||% list()) > 0) {
                clust_origins <- .cv$props$origins
                break
              }
            }
          }
          if (!is.null(clust_origins)) {
            lefts  <- vapply(clust_origins, `[[`, integer(1), "col")
            offset <- c - lefts[1]
            equiv  <- lefts + offset  # one entry per block, preserving duplicates
            p$columns <- if (c %in% cols) integer(0) else equiv
          } else {
            p$columns <- if (c %in% cols) setdiff(cols, c) else sort(c(cols, c))
          }
        }
      } else if (mode == "row") {
        rows <- p$rows %||% integer(0)
        p$rows <- if (r %in% rows) setdiff(rows, r) else sort(c(rows, r))
      }
      v[[idx]]$props <- p
      rv$vars <- v
    })

    # -- reorder variables by drag-and-drop ----------------------------------
    shiny::observeEvent(input$sb_var_reorder, {
      from <- input$sb_var_reorder$from
      to   <- input$sb_var_reorder$to
      v    <- rv$vars
      n    <- length(v)
      if (is.null(from) || is.null(to) || from == to ||
          from < 1 || from > n || to < 1 || to > n) return()
      # insert `from` element before or after `to` depending on direction
      item   <- v[[from]]
      v      <- v[-from]
      to_adj <- if (to > from) to - 1L else to
      rv$vars <- append(v, list(item), after = to_adj - 1L)
    })

    # -- add new variable ----------------------------------------------------
    shiny::observeEvent(input$sb_new_id, {
      n   <- length(rv$vars) + 1L
      key <- paste0("id_", n)
      rv$vars <- c(rv$vars, list(list(
        key   = key,
        type  = "id",
        step  = 1L,
        done  = FALSE,
        props = list(name = "")
      )))
      rv$active_key <- key
      rv$grid_mode  <- "none"
    })

    shiny::observeEvent(input$sb_new_obs, {
      n   <- length(rv$vars) + 1L
      key <- paste0("obs_", n)
      rv$vars <- c(rv$vars, list(list(
        key   = key,
        type  = "obs",
        step  = 1L,
        done  = FALSE,
        props = list(name = "")
      )))
      rv$active_key <- key
      rv$grid_mode  <- "none"
    })

    # -- remove variable -----------------------------------------------------
    for (.n in seq_len(40)) {
      local({
        n <- .n
        shiny::observeEvent(input[[paste0("sb_remove_var_", n)]], {
          v <- rv$vars
          if (n <= length(v)) {
            removed_key <- v[[n]]$key
            rv$vars <- v[-n]
            if (identical(rv$active_key, removed_key))
              rv$active_key <- NULL
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }

    # -- select active variable ----------------------------------------------
    for (.n in seq_len(40)) {
      local({
        n <- .n
        shiny::observeEvent(input[[paste0("sb_select_var_", n)]], {
          v <- rv$vars
          if (n <= length(v)) {
            var <- v[[n]]
            rv$active_key <- var$key
            step_key  <- paste0("step", var$step)
            prior_ans <- var$props[[step_key]]
            rv$grid_mode <- .sb_grid_mode(var$type, var$step, prior_ans, var$props,
                                          .hla(rv$vars, var$key), listed_answer = .hla_answer(rv$vars, var$key),
                                          cluster_obs_answer = .hco_answer(rv$vars, var$key),
                                          n_other_obs = .n_other_obs(rv$vars, var$key), n_clusters = .n_clusters(rv$vars))
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }

    # -- mini-tree navigation ------------------------------------------------
    shiny::observeEvent(input$sb_step_next, {
      key <- rv$active_key; if (is.null(key)) return()
      v   <- rv$vars
      idx <- which(vapply(v, function(x) x$key == key, logical(1)))
      if (length(idx) == 0) return()
      # save text inputs on Next (not via live observer, to avoid focus loss)
      cur_steps <- .sb_steps(v[[idx]]$type, v[[idx]]$props,
                              v[[idx]]$step, .hla(v, v[[idx]]$key), listed_answer = .hla_answer(v, v[[idx]]$key),
                              cluster_obs_answer = .hco_answer(v, v[[idx]]$key),
                              n_other_obs = .n_other_obs(v, v[[idx]]$key), n_clusters = .n_clusters(v))
      if (v[[idx]]$step <= length(cur_steps)) {
        cur_ui <- cur_steps[[v[[idx]]$step]]$ui
        if (cur_ui == "text_name") {
          nm <- input$sb_var_name
          if (!is.null(nm)) {
            step_key <- cur_steps[[v[[idx]]$step]]$key
            if (identical(step_key, "group_name"))
              v[[idx]]$props$group_name <- nm
            else
              v[[idx]]$props$name <- nm
          }
        } else if (cur_ui %in% c("text_value", "yn_then_text_merge", "yn_then_text_split", "num_factor")) {
          prop_key <- switch(cur_ui,
            yn_then_text_merge = "merge_char",
            yn_then_text_split = "split_regex",
            num_factor         = "factor",
            cur_steps[[v[[idx]]$step]]$key)
          if (!is.null(prop_key)) v[[idx]]$props[[prop_key]] <- input$sb_text_answer
        } else if (cur_ui == "key_value_select") {
          val <- input$sb_listed_value %||% input$sb_text_answer
          if (!is.null(val)) v[[idx]]$props$listed_value <- val
        } else if (cur_ui == "cluster_num_select") {
          val <- suppressWarnings(as.integer(input$sb_cluster_num))
          if (!is.na(val)) v[[idx]]$props$cluster_value <- val
        } else if (cur_ui == "member_assign") {
          n_origs <- length(v[[idx]]$props$origins %||% list())
          member  <- vapply(seq_len(n_origs), function(i) {
            suppressWarnings(as.integer(input[[paste0("sb_member_", i)]] %||% 1L))
          }, integer(1))
          member[is.na(member)] <- 1L
          v[[idx]]$props$member <- member
        }
      }
      n_steps  <- length(cur_steps)
      cur_step <- v[[idx]]$step
      if (cur_step >= n_steps) {
        # final "Done" click -- mark complete, collapse form
        v[[idx]]$done <- TRUE
        rv$vars       <- v
        rv$active_key <- NULL
        rv$grid_mode  <- "none"
      } else {
        new_step <- cur_step + 1L
        v[[idx]]$step <- new_step
        rv$vars <- v
        rv$grid_mode <- .sb_grid_mode(v[[idx]]$type, new_step, answer = NULL,
                                      v[[idx]]$props, .hla(v, v[[idx]]$key), listed_answer = .hla_answer(v, v[[idx]]$key),
                                      cluster_obs_answer = .hco_answer(v, v[[idx]]$key),
                                      n_other_obs = .n_other_obs(v, v[[idx]]$key), n_clusters = .n_clusters(v))
        # Clear the shared radio so the stale value doesn't fire into the next step's prop
        shiny::updateRadioButtons(session, "sb_yn_answer", selected = character(0))
      }
    })

    shiny::observeEvent(input$sb_step_back, {
      key <- rv$active_key; if (is.null(key)) return()
      v   <- rv$vars
      idx <- which(vapply(v, function(x) x$key == key, logical(1)))
      if (length(idx) == 0) return()
      # save text inputs on Back too, so content isn't lost when stepping back
      back_steps <- .sb_steps(v[[idx]]$type, v[[idx]]$props,
                               v[[idx]]$step, .hla(v, v[[idx]]$key), listed_answer = .hla_answer(v, v[[idx]]$key),
                               cluster_obs_answer = .hco_answer(v, v[[idx]]$key),
                               n_other_obs = .n_other_obs(v, v[[idx]]$key), n_clusters = .n_clusters(v))
      if (v[[idx]]$step <= length(back_steps)) {
        back_ui <- back_steps[[v[[idx]]$step]]$ui
        if (back_ui == "text_name") {
          nm <- input$sb_var_name
          if (!is.null(nm) && nchar(nm) > 0) v[[idx]]$props$name <- nm
        } else if (back_ui %in% c("text_value", "yn_then_text_merge", "yn_then_text_split", "num_factor")) {
          prop_key <- switch(back_ui,
            yn_then_text_merge = "merge_char",
            yn_then_text_split = "split_regex",
            num_factor         = "factor",
            back_steps[[v[[idx]]$step]]$key)
          if (!is.null(prop_key)) v[[idx]]$props[[prop_key]] <- input$sb_text_answer
        } else if (back_ui == "key_value_select") {
          val <- input$sb_listed_value %||% input$sb_text_answer
          if (!is.null(val)) v[[idx]]$props$listed_value <- val
        } else if (back_ui == "cluster_num_select") {
          val <- suppressWarnings(as.integer(input$sb_cluster_num))
          if (!is.na(val)) v[[idx]]$props$cluster_value <- val
        } else if (back_ui == "member_assign") {
          n_origs <- length(v[[idx]]$props$origins %||% list())
          member  <- vapply(seq_len(n_origs), function(i) {
            suppressWarnings(as.integer(input[[paste0("sb_member_", i)]] %||% 1L))
          }, integer(1))
          member[is.na(member)] <- 1L
          v[[idx]]$props$member <- member
        }
      }
      new_step <- max(1L, v[[idx]]$step - 1L)
      v[[idx]]$step <- new_step
      rv$vars <- v
      # restore grid mode for the step we're returning to
      step_key <- paste0("step", new_step)
      prior_ans <- v[[idx]]$props[[step_key]]
      rv$grid_mode <- .sb_grid_mode(v[[idx]]$type, new_step, prior_ans,
                                    v[[idx]]$props, .hla(v, v[[idx]]$key), listed_answer = .hla_answer(v, v[[idx]]$key),
                                    cluster_obs_answer = .hco_answer(v, v[[idx]]$key),
                                    n_other_obs = .n_other_obs(v, v[[idx]]$key), n_clusters = .n_clusters(v))
      shiny::updateRadioButtons(session, "sb_yn_answer", selected = character(0))
    })

    # -- yes/no answers in mini-tree -----------------------------------------
    shiny::observeEvent(input$sb_yn_answer, {
      key <- rv$active_key; if (is.null(key)) return()
      ans <- input$sb_yn_answer
      if (is.null(ans) || length(ans) == 0 || !ans %in% c("yes", "no")) return()
      v   <- rv$vars
      idx <- which(vapply(v, function(x) x$key == key, logical(1)))
      if (length(idx) == 0) return()
      step  <- v[[idx]]$step
      # look up which prop key this step writes to
      steps <- .sb_steps(v[[idx]]$type, v[[idx]]$props, v[[idx]]$step, .hla(v, v[[idx]]$key), listed_answer = .hla_answer(v, v[[idx]]$key),
                         cluster_obs_answer = .hco_answer(v, v[[idx]]$key),
                         n_other_obs = .n_other_obs(v, v[[idx]]$key), n_clusters = .n_clusters(v))
      n     <- length(steps)
      if (step >= 1L && step <= n) {
        # ignore stale radio fires when current step is not a yn-type
        yn_uis <- c("yn", "yn_then_row", "yn_then_cols", "yn_then_col1",
                    "yn_then_text_merge", "yn_then_text_split", "yn_then_origins", "yn_then_kv")
        if (!steps[[step]]$ui %in% yn_uis) return()
        prop_key <- steps[[step]]$key
        if (!is.null(prop_key)) v[[idx]]$props[[prop_key]] <- ans
        # When "no" is selected on a yn_then_kv step, clear key/value col selections
        # AND any p$columns that may have been set by an accidental click while "yes" was active
        if (steps[[step]]$ui == "yn_then_kv" && ans == "no") {
          v[[idx]]$props$key_col <- NULL
          v[[idx]]$props$val_col <- NULL
          v[[idx]]$props$columns <- NULL
        }
      }
      # set grid mode -- pass updated props so branch is reflected immediately
      rv$grid_mode <- .sb_grid_mode(v[[idx]]$type, step, ans,
                                    v[[idx]]$props, .hla(v, v[[idx]]$key), listed_answer = .hla_answer(v, v[[idx]]$key),
                                    cluster_obs_answer = .hco_answer(v, v[[idx]]$key),
                                    n_other_obs = .n_other_obs(v, v[[idx]]$key), n_clusters = .n_clusters(v))
      rv$vars <- v
    })


    # save text field answers (factor, injected value, merge char, split regex, etc.)
    # sb_text_answer is saved on Next/Back (not live) to avoid re-render focus loss

    # -- phase navigation ----------------------------------------------------
    shiny::observeEvent(input$sb_to_phase2, { rv$phase <- 2L })
    shiny::observeEvent(input$sb_back_to_phase1, {
      rv$phase        <- 1L
      rv$vars         <- list()
      rv$active_key   <- NULL
      rv$grid_mode    <- "none"
    })

    # -- phase 3: flags table -------------------------------------------------
    # Format text fields (decimal, thousand, na, zero, header) are NOT stored in rv --
    # they are read directly from input$* at code-gen time to avoid re-render focus loss.
    shiny::observeEvent(input$sb_add_flag, {
      rv$fmt_flags <- rbind(rv$fmt_flags,
        data.frame(flag = "", value = "", stringsAsFactors = FALSE))
    })

    # Flag row deletes only (edits are read from input$* at code-gen time)
    for (.n in seq_len(20)) {
      local({
        n <- .n
        shiny::observeEvent(input[[paste0("sb_del_flag_", n)]], {
          fl <- rv$fmt_flags
          if (n <= nrow(fl)) rv$fmt_flags <- fl[-n, , drop = FALSE]
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }

    # -- phase 3: filter observers -------------------------------------------
    shiny::observeEvent(input$sb_add_filter, {
      rv$filters <- c(rv$filters, list(list(
        mode         = "click",   # "click" or "find"
        click_dim    = "row",     # "row" or "col" -- persisted so radio doesn't snap back
        rows         = integer(0),
        cols         = integer(0),
        invert       = FALSE,
        use_find       = FALSE,
        find_pattern   = "",
        find_search_in = "row",    # which dimension to scan: "row" or "col"
        find_dim       = "",       # specific row/col number to scan (optional)
        find_select    = "rows",   # what setFilter receives: "rows" or "columns"
        operator       = "&"
      )))
      # switch grid mode so user can start clicking
      if (length(rv$filters) > 0)
        rv$grid_mode <- "filter_row"
    })

    for (.n in seq_len(10)) {
      local({
        n <- .n
        # mode toggle
        shiny::observeEvent(input[[paste0("flt_mode_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) {
            fl[[n]]$mode <- input[[paste0("flt_mode_", n)]]
            fl[[n]]$use_find <- (fl[[n]]$mode == "find")
            rv$filters <- fl
            # update grid mode when switching modes
            rv$grid_mode <- if (fl[[n]]$mode == "click") "filter_row" else "none"
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
        # click sub-mode (row/col)
        shiny::observeEvent(input[[paste0("flt_click_dim_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) {
            dim <- input[[paste0("flt_click_dim_", n)]]
            fl[[n]]$click_dim <- dim
            rv$filters   <- fl
            rv$grid_mode <- if (dim == "row") "filter_row" else "filter_col"
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
        # finder fields
        shiny::observeEvent(input[[paste0("flt_pattern_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) { fl[[n]]$find_pattern <- input[[paste0("flt_pattern_", n)]]; rv$filters <- fl }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("flt_in_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) { fl[[n]]$find_search_in <- input[[paste0("flt_in_", n)]]; rv$filters <- fl }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("flt_select_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) { fl[[n]]$find_select <- input[[paste0("flt_select_", n)]]; rv$filters <- fl }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("flt_dim_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) { fl[[n]]$find_dim <- input[[paste0("flt_dim_", n)]]; rv$filters <- fl }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        # invert is read from input$flt_invert_N at render/codegen time -- no rv write needed
        shiny::observeEvent(input[[paste0("flt_op_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) { fl[[n]]$operator <- input[[paste0("flt_op_", n)]]; rv$filters <- fl }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("sb_del_filter_", n)]], {
          fl <- rv$filters
          if (n <= length(fl)) rv$filters <- fl[-n]
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }

    # -- phase 1: row group observers ----------------------------------------
    shiny::observeEvent(input$sb_add_group, {
      n <- length(rv$groups) + 1L
      rv$groups <- c(rv$groups, list(list(rows = integer(0))))
      rv$active_group <- n
      rv$grid_mode    <- "group_row"
    })

    for (.n in seq_len(10)) {
      local({
        n <- .n
        shiny::observeEvent(input[[paste0("sb_del_group_", n)]], {
          grps <- rv$groups
          if (n <= length(grps)) {
            rv$groups <- grps[-n]
            if (identical(rv$active_group, n)) {
              rv$active_group <- NULL
              rv$grid_mode    <- "none"
            }
          }
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("sb_edit_group_", n)]], {
          rv$active_group <- n
          rv$grid_mode    <- "group_row"
        }, ignoreNULL = TRUE, ignoreInit = TRUE)
      })
    }

    # -- phase 1: row groups list UI -----------------------------------------
    output$sb_groups_ui <- shiny::renderUI({
      if (length(rv$groups) == 0) return(NULL)
      group_items <- lapply(seq_along(rv$groups), function(i) {
        grp       <- rv$groups[[i]]
        is_active <- identical(rv$active_group, i)
        row_label <- if (length(grp$rows) > 0) paste(grp$rows, collapse=", ")
                     else "none"

        if (!is_active && length(grp$rows) > 0) {
          # collapsed view for finished groups
          shiny::tags$div(
            style = "border:1px solid #ddd;border-radius:6px;padding:6px 10px;margin-bottom:8px;background:#fafafa;display:flex;justify-content:space-between;align-items:center;",
            shiny::tags$span(style="font-size:12px;color:#555;",
              shiny::tags$strong(paste0("Group ", i, ": ")),
              paste0("rows ", row_label)),
            shiny::tags$div(
              shiny::tags$button(class="sb-btn-sm", style="margin-right:4px;",
                onclick=sprintf("Shiny.setInputValue('sb_edit_group_%d', Math.random(), {priority:'event'})", i),
                "Expand"),
              shiny::tags$button(class="sb-btn-sm", style="color:#c0392b;padding:3px 8px;",
                onclick=sprintf("Shiny.setInputValue('sb_del_group_%d', Math.random(), {priority:'event'})", i),
                "Remove")
            )
          )
        } else {
          # expanded/active view
          shiny::tags$div(
            style = paste0("border:1px solid ", if (is_active) "#5B9BD5" else "#ddd",
                           ";border-radius:6px;padding:10px;margin-bottom:8px;background:",
                           if (is_active) "#f0f6fd" else "#fafafa", ";"),
            shiny::tags$div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:6px;",
              shiny::tags$span(style="font-weight:600;font-size:12px;",
                paste0("Group ", i, if (is_active) " \u2014 click rows to select" else "")),
              shiny::tags$div(
                if (!is_active)
                  shiny::tags$button(class="sb-btn-sm", style="margin-right:4px;",
                    onclick=sprintf("Shiny.setInputValue('sb_edit_group_%d', Math.random(), {priority:'event'})", i),
                    "Edit"),
                shiny::tags$button(class="sb-btn-sm", style="color:#c0392b;padding:3px 8px;",
                  onclick=sprintf("Shiny.setInputValue('sb_del_group_%d', Math.random(), {priority:'event'})", i),
                  "Remove")
              )
            ),
            shiny::tags$p(style="font-size:12px;color:#555;margin:0 0 6px;",
              paste0("Rows: ", if (length(grp$rows) > 0) row_label else "none \u2014 click rows in the table")),
            shiny::tags$div(style="display:flex;align-items:center;gap:16px;margin-bottom:4px;",
              shiny::tags$label(style="font-size:12px;color:#555;white-space:nowrap;", "Fill NAs:"),
              shiny::tags$div(style="display:flex;gap:12px;",
                shiny::checkboxInput(paste0("grp_fill_down_",  i), "down",  width="auto"),
                shiny::checkboxInput(paste0("grp_fill_up_",    i), "up",    width="auto"),
                shiny::checkboxInput(paste0("grp_fill_right_", i), "right", width="auto")
              )
            ),
            shiny::tags$details(
              shiny::tags$summary(style="font-size:12px;color:#888;cursor:pointer;", "Advanced aggregation functions"),
              shiny::tags$div(style="margin-top:6px;display:flex;gap:12px;flex-wrap:wrap;",
                shiny::tags$div(
                  shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:2px;",
                    "Character columns (R function body)"),
                  shiny::tags$input(
                    id          = paste0("grp_char_fn_", i),
                    type        = "text",
                    class       = "form-control",
                    placeholder = "e.g. paste0(na.omit(.x), collapse=' ')",
                    style       = "width:260px;font-size:13px;padding:4px 6px;",
                    oninput     = sprintf("Shiny.setInputValue('grp_char_fn_live_%d', this.value, {priority:'event'})", i)
                  ),
                  shiny::uiOutput(paste0("sb_grp_char_fb_", i))
                ),
                shiny::tags$div(
                  shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:2px;",
                    "Numeric columns (R function body)"),
                  shiny::tags$input(
                    id          = paste0("grp_num_fn_", i),
                    type        = "text",
                    class       = "form-control",
                    placeholder = "e.g. sum(.x, na.rm=TRUE)",
                    style       = "width:200px;font-size:13px;padding:4px 6px;",
                    oninput     = sprintf("Shiny.setInputValue('grp_num_fn_live_%d', this.value, {priority:'event'})", i)
                  ),
                  shiny::uiOutput(paste0("sb_grp_num_fb_", i))
                )
              )
            )
          )
        }
      })
      all_grp_rows  <- unlist(lapply(rv$groups, `[[`, "rows"))
      conflict_rows <- all_grp_rows[duplicated(all_grp_rows)]
      conflict_warn <- if (length(conflict_rows) > 0)
        shiny::tags$p(style="font-size:12px;color:#c0392b;margin-top:6px;",
          paste0("\u26a0 Rows in multiple groups: ", paste(conflict_rows, collapse=", ")))
      else NULL

      shiny::tagList(do.call(shiny::tagList, group_items), conflict_warn)
    })


    # grid clicks for filter/group mode (phase 1)
    shiny::observeEvent(input$sb_cell, {
      if (rv$phase != 1L) return()
      r <- input$sb_cell$row
      c <- input$sb_cell$col
      if (rv$grid_mode == "filter_row") {
        # find which filter is being edited -- last one by default
        n  <- length(rv$filters); if (n == 0) return()
        fl <- rv$filters
        fl[[n]]$rows <- if (r %in% fl[[n]]$rows) setdiff(fl[[n]]$rows, r)
                        else sort(c(fl[[n]]$rows, r))
        rv$filters <- fl
      } else if (rv$grid_mode == "filter_col") {
        n  <- length(rv$filters); if (n == 0) return()
        fl <- rv$filters
        fl[[n]]$cols <- if (c %in% fl[[n]]$cols) setdiff(fl[[n]]$cols, c)
                        else sort(c(fl[[n]]$cols, c))
        rv$filters <- fl
      } else if (rv$grid_mode == "group_row") {
        n <- rv$active_group; if (is.null(n) || n > length(rv$groups)) return()
        grps <- rv$groups
        grps[[n]]$rows <- if (r %in% grps[[n]]$rows) setdiff(grps[[n]]$rows, r)
                          else sort(c(grps[[n]]$rows, r))
        rv$groups <- grps
      }
    }, priority = 10)

    # -- progress dots -------------------------------------------------------
    output$sb_dots <- shiny::renderUI({
      steps <- c("Setup", "Variables")
      items <- lapply(seq_along(steps), function(i) {
        dot_cls <- if (i < rv$phase) "sb-dot done"
                   else if (i == rv$phase) "sb-dot active"
                   else "sb-dot"
        lbl_col <- if (i == rv$phase) "#333" else "#aaa"
        shiny::tags$div(
          style = "display:flex;flex-direction:column;align-items:center;gap:4px;width:80px;",
          shiny::tags$div(class = dot_cls),
          shiny::tags$span(style = paste0("font-size:11px;color:", lbl_col, ";text-align:center;"), steps[i])
        )
      })
      shiny::tags$div(style = "display:flex;gap:0;", items)
    })

    # -- phase 1: setup (format, row groups, filter) -------------------------
    output$sb_phase1 <- shiny::renderUI({
      if (rv$phase != 1L) return(NULL)
      if (is.null(rv$tbl)) return(shiny::tags$div(class="sb-panel",
        shiny::tags$p("Upload your table on the right to begin.")))

      # Format section
      format_section <- shiny::tags$div(
        style = "margin-bottom:16px;",
        shiny::tags$h4(style="margin-bottom:10px;", "Format options"),
        shiny::tags$p(style="font-size:12px;color:#666;margin-bottom:10px;",
          "These are optional. Leave blank to use defaults."),
        shiny::tags$div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:10px;",
          shiny::tags$div(
            shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
              "Decimal separator"),
            shiny::textInput("fmt_decimal", NULL, value = "", placeholder = "e.g. ,", width = "80px")
          ),
          shiny::tags$div(
            shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
              "Thousand separator"),
            shiny::textInput("fmt_thousand", NULL, value = "", placeholder = "e.g. .", width = "80px")
          )
        ),
        shiny::tags$div(style="display:flex;gap:12px;flex-wrap:wrap;margin-bottom:10px;",
          shiny::tags$div(
            shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
              "NA values (comma-separated)"),
            shiny::textInput("fmt_na", NULL, value = "", placeholder = "e.g. NA, -99, n/a", width = "200px")
          ),
          shiny::tags$div(
            shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
              "Zero values (comma-separated)"),
            shiny::textInput("fmt_zero", NULL, value = "", placeholder = "e.g. 0, -", width = "200px")
          )
        ),
        shiny::tags$div(
          shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:6px;",
            "Flags (character prefixes/suffixes on numeric values)"),
          shiny::uiOutput("sb_fmt_flags_ui")
        ),
        shiny::tags$button(class="sb-btn-sm",
          onclick="Shiny.setInputValue('sb_add_flag', Math.random(), {priority:'event'})",
          "+ Add flag")
      )

      # Row groups section
      groups_section <- shiny::tags$div(
        style = "margin-bottom:16px;",
        shiny::tags$h4(style="margin-bottom:6px;", "Row groups"),
        shiny::tags$p(style="font-size:12px;color:#666;margin-bottom:10px;",
          "Use this when multiple rows need to be collapsed into one before variable extraction (e.g. split header rows, multi-line records)."),
        shiny::uiOutput("sb_groups_ui"),
        shiny::tags$button(class="sb-btn-sm",
          onclick="Shiny.setInputValue('sb_add_group', Math.random(), {priority:'event'})",
          "+ Add row group")
      )

      # Filter section
      filter_section <- shiny::tags$div(
        shiny::tags$h4(style="margin-bottom:6px;", "Filters"),
        shiny::tags$p(style="font-size:12px;color:#666;margin-bottom:10px;",
          "Optionally exclude specific rows or columns. Each filter can target rows/columns by clicking or by regex pattern."),
        shiny::uiOutput("sb_filters_ui"),
        shiny::tags$button(class = "sb-btn-sm",
          onclick = "Shiny.setInputValue('sb_add_filter', Math.random(), {priority:'event'})",
          "+ Add filter")
      )

      shiny::tags$div(class = "sb-panel",
        shiny::tags$div(class = "sb-phase sb-phase-1", "Phase 1 \u2014 Setup"),
        format_section,
        shiny::tags$hr(),
        groups_section,
        shiny::tags$hr(),
        filter_section,
        shiny::tags$hr(),
        shiny::tags$button(class="sb-btn sb-btn-primary",
          onclick="Shiny.setInputValue('sb_to_phase2', Math.random(), {priority:'event'})",
          "Continue \u2192 Describe variables")
      )
    })

    # -- phase 2: variable interview -----------------------------------------
    output$sb_phase2 <- shiny::renderUI({
      if (rv$phase != 2L) return(NULL)
      shiny::req(rv$tbl)

      v          <- rv$vars
      active_key <- rv$active_key
      type       <- rv$table_type %||% "tidy"

      # --- variable list (pills) ---
      pill_items <- lapply(seq_along(v), function(i) {
        var          <- v[[i]]
        nm           <- if (nchar(var$props$name %||% "") > 0) var$props$name
                        else paste0(var$type, "_", i)
        is_active    <- identical(var$key, active_key)
        is_done      <- isTRUE(var$done)
        pill_cls <- paste0("sb-var-btn sb-var-", var$type,
                           if (is_active) " sb-var-active" else "")
        label    <- if (is_done) paste0("\u2713 ", nm) else nm
        shiny::tags$span(
          class      = pill_cls,
          draggable  = "true",
          `data-idx` = i,
          style   = "cursor:grab;",
          title   = if (is_done) "Click to re-edit \u2014 drag to reorder" else "Click to edit \u2014 drag to reorder",
          onclick = sprintf(
            "Shiny.setInputValue('sb_select_var_%d', Math.random(), {priority:'event'})", i),
          shiny::tags$span(label),
          shiny::tags$span(
            style   = "margin-left:6px;opacity:.6;font-size:11px;cursor:pointer;",
            onclick = sprintf(
              "event.stopPropagation(); Shiny.setInputValue('sb_remove_var_%d', Math.random(), {priority:'event'})", i),
            "\u2715")
        )
      })

      # --- active variable mini-tree (only if not done) ---
      active_panel <- if (!is.null(active_key)) {
        idx <- which(vapply(v, function(x) x$key == active_key, logical(1)))
        if (length(idx) > 0 && !isTRUE(v[[idx]]$done)) {
          var  <- v[[idx]]
          hla  <- any(vapply(v[-idx], function(x)
            x$type == "obs" && isTRUE(x$props$listed == "yes"), logical(1)))
          lkc  <- NULL
          for (.lv in v) {
            if (.lv$type == "obs" && isTRUE(.lv$props$listed == "yes")) {
              lkc <- .lv$props$key_col; break
            }
          }
          .sb_var_mini_tree(var$type, var$step, var$props, hla, tbl = rv$tbl, listed_key_col = lkc, listed_answer = .hla_answer(v, var$key),
                            cluster_obs_answer = .hco_answer(v, var$key),
                            cluster_obs_origins = .hco_origins(v, var$key),
                            n_other_obs = .n_other_obs(v, var$key), n_clusters = .n_clusters(v))
        }
      }


      shiny::tags$div(class = "sb-panel",
        shiny::tags$div(class = "sb-phase sb-phase-2", "Phase 2 \u2014 Variables"),
        shiny::tags$p(
          style = "font-size:12px;color:#666;margin-bottom:10px;",
          "Declare each variable. Click a pill to edit it. Click cells/columns in the table on the right when prompted."),

        # Add buttons
        shiny::tags$div(style = "margin-bottom:10px;",
          shiny::tags$button(class = "sb-btn-sm",
            style = "background:#deeaf7;color:#2a5ca8;border:1px solid #5B9BD5;",
            onclick = "Shiny.setInputValue('sb_new_id', Math.random(), {priority:'event'})",
            "+ ID variable"),
          shiny::tags$button(class = "sb-btn-sm",
            style = "background:#fde9d9;color:#a0440a;border:1px solid #ED7D31;margin-left:6px;",
            onclick = "Shiny.setInputValue('sb_new_obs', Math.random(), {priority:'event'})",
            "+ Observed variable")
        ),

        # Variable pills
        if (length(v) > 0) shiny::tags$div(
          style = "margin-bottom:12px;",
          do.call(shiny::tagList, pill_items)
        ),

        # Active variable panel
        if (!is.null(active_panel)) active_panel,

        shiny::tags$hr(),
        shiny::tags$button(class = "sb-btn-sm",
          onclick = "Shiny.setInputValue('sb_back_to_phase1', Math.random(), {priority:'event'})",
          "\u2190 Back to setup")
      )
    })

    # -- phase 3: flags table (separate renderUI so edits don't re-render the whole panel) ---
    output$sb_fmt_flags_ui <- shiny::renderUI({
      flag_rows <- lapply(seq_len(nrow(rv$fmt_flags)), function(i) {
        fl <- rv$fmt_flags
        shiny::tags$div(
          # fixed layout: flag input, value input, delete button -- no reflow
          style = "display:table-row;",
          shiny::tags$div(style = "display:table-cell;padding-right:6px;padding-bottom:4px;width:130px;",
            shiny::tags$input(id = paste0("fmt_flag_", i), type = "text",
              value = fl$flag[i], placeholder = "flag",
              class = "form-control",
              style = "width:130px;font-size:13px;padding:4px 6px;",
              onchange = sprintf("Shiny.setInputValue('fmt_flag_%d', this.value, {priority:'event'})", i))
          ),
          shiny::tags$div(style = "display:table-cell;padding-right:6px;padding-bottom:4px;width:180px;",
            shiny::tags$input(id = paste0("fmt_flagval_", i), type = "text",
              value = fl$value[i], placeholder = "meaning",
              class = "form-control",
              style = "width:180px;font-size:13px;padding:4px 6px;",
              onchange = sprintf("Shiny.setInputValue('fmt_flagval_%d', this.value, {priority:'event'})", i))
          ),
          shiny::tags$div(style = "display:table-cell;padding-bottom:4px;vertical-align:middle;",
            shiny::tags$button(class = "sb-btn-sm",
              style = "color:#c0392b;padding:3px 8px;",
              onclick = sprintf("Shiny.setInputValue('sb_del_flag_%d', Math.random(), {priority:'event'})", i),
              "\u2715")
          )
        )
      })
      if (length(flag_rows) > 0)
        shiny::tags$div(style = "display:table;", do.call(shiny::tagList, flag_rows))
    })

    # Flag value saves (onchange events fire these)
    for (.n in seq_len(20)) {
      local({
        n <- .n
        shiny::observeEvent(input[[paste0("fmt_flag_", n)]], {
          fl <- rv$fmt_flags
          if (n <= nrow(fl)) { fl$flag[n]  <- input[[paste0("fmt_flag_",    n)]]; rv$fmt_flags <- fl }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
        shiny::observeEvent(input[[paste0("fmt_flagval_", n)]], {
          fl <- rv$fmt_flags
          if (n <= nrow(fl)) { fl$value[n] <- input[[paste0("fmt_flagval_", n)]]; rv$fmt_flags <- fl }
        }, ignoreNULL = FALSE, ignoreInit = TRUE)
      })
    }


    # -- phase 3: filter list (separate renderUI -- re-renders on rv$filters change only) ---
    output$sb_filters_ui <- shiny::renderUI({
      if (length(rv$filters) == 0) return(NULL)
      filter_items <- lapply(seq_along(rv$filters), function(i) {
        flt     <- rv$filters[[i]]
        is_find <- isTRUE(flt$use_find)
        op_row <- if (i > 1)
          shiny::tags$div(style="margin-bottom:6px;",
            shiny::tags$label(style="font-size:12px;color:#555;", "Combine with previous filter:"),
            shiny::radioButtons(paste0("flt_op_", i), NULL,
              choices = c("AND" = "&", "OR" = "|"),
              selected = flt$operator %||% "&",
              inline = TRUE))
        else NULL

        click_content <- shiny::tagList(
          shiny::tags$div(style="display:flex;gap:8px;align-items:center;margin-bottom:6px;",
            shiny::tags$span(style="font-size:12px;color:#555;", "Click to exclude:"),
            shiny::radioButtons(paste0("flt_click_dim_", i), NULL,
              choices = c("Rows" = "row", "Columns" = "col"),
              selected = flt$click_dim %||% "row", inline = TRUE)
          ),
          shiny::tags$p(style="font-size:12px;color:#555;margin:0;",
            paste0("Excluded rows: ",
              if (length(flt$rows) > 0) paste(flt$rows, collapse=", ") else "none \u2014 click rows in the table")),
          shiny::tags$p(style="font-size:12px;color:#555;margin:0;",
            paste0("Excluded cols: ",
              if (length(flt$cols) > 0) paste(flt$cols, collapse=", ") else "none"))
        )

        find_err <- if (is_find && nchar(flt$find_pattern) > 0)
          tryCatch({ grepl(flt$find_pattern, ""); NULL },
                   error = function(e) paste("Regex error:", conditionMessage(e)))
        else NULL

        search_in <- flt$find_search_in %||% "row"
        find_content <- shiny::tagList(
          shiny::tags$div(style="display:flex;gap:8px;flex-wrap:wrap;align-items:flex-end;margin-bottom:6px;",
            shiny::tags$div(
              shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
                "Regex pattern"),
              shiny::textInput(paste0("flt_pattern_", i), NULL,
                value = flt$find_pattern, placeholder = "e.g. ^NA$", width = "160px")
            ),
            shiny::tags$div(
              shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
                "Search in"),
              shiny::selectInput(paste0("flt_in_", i), NULL,
                choices = c("Rows" = "row", "Columns" = "col"),
                selected = search_in, width = "100px")
            ),
            shiny::tags$div(
              shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
                paste0(if (search_in == "col") "Column" else "Row", " number (optional)")),
              shiny::textInput(paste0("flt_dim_", i), NULL,
                value = flt$find_dim, placeholder = "e.g. 1", width = "80px")
            ),
            shiny::tags$div(
              shiny::tags$label(style="font-size:12px;color:#555;display:block;margin-bottom:3px;",
                "Select"),
              shiny::selectInput(paste0("flt_select_", i), NULL,
                choices = c("Rows" = "rows", "Columns" = "columns"),
                selected = flt$find_select %||% "rows", width = "100px")
            )
          ),
          if (!is.null(find_err))
            shiny::tags$p(style="font-size:12px;color:#c0392b;", find_err)
          else if (nchar(flt$find_pattern) > 0)
            shiny::tags$p(style="font-size:12px;color:#2d6a16;", "\u2713 Valid regex \u2014 matching cells highlighted in table")
        )

        shiny::tags$div(
          style = "border:1px solid #ddd;border-radius:6px;padding:10px;margin-bottom:8px;background:#fafafa;",
          op_row,
          shiny::tags$div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:8px;",
            shiny::tags$span(style="font-weight:600;font-size:12px;", paste0("Filter ", i)),
            shiny::tags$button(class="sb-btn-sm", style="color:#c0392b;padding:3px 8px;",
              onclick=sprintf("Shiny.setInputValue('sb_del_filter_%d', Math.random(), {priority:'event'})", i),
              "Remove")
          ),
          shiny::tags$div(style="margin-bottom:8px;",
            shiny::radioButtons(paste0("flt_mode_", i), NULL,
              choices = c("Click to exclude" = "click", "Use finder (.find)" = "find"),
              selected = if (is_find) "find" else "click", inline = TRUE)
          ),
          if (is_find) find_content else click_content,
          shiny::tags$div(style="margin-top:6px;",
            shiny::checkboxInput(paste0("flt_invert_", i),
              "Invert (keep the clicked rows/cols instead of excluding them)",
              value = isTRUE(flt$invert)))
        )
      })
      do.call(shiny::tagList, filter_items)
    })


    # -- grid output (sticky right) ------------------------------------------
    output$sb_grid_out <- shiny::renderUI({
      shiny::req(rv$tbl)
      nr   <- nrow(rv$tbl)
      nc   <- ncol(rv$tbl)
      hl   <- list()
      v    <- rv$vars

      # phase 1 row groups -- each group gets a distinct tint; conflicts in red
      grp_palette <- c("#e8f4d9", "#fef9c3", "#e8eaf6", "#fce4ec", "#e0f7fa",
                       "#f3e5f5", "#fff3e0", "#e8f5e9", "#fbe9e7", "#e3f2fd")
      all_grp_rows <- unlist(lapply(rv$groups, `[[`, "rows"))
      conflict_grp <- all_grp_rows[duplicated(all_grp_rows)]
      for (gi in seq_along(rv$groups)) {
        grp_rows <- rv$groups[[gi]]$rows
        if (length(grp_rows) == 0) next
        ok_rows  <- setdiff(grp_rows, conflict_grp)
        col_grp  <- grp_palette[((gi - 1L) %% length(grp_palette)) + 1L]
        if (length(ok_rows) > 0)
          hl <- c(hl, list(list(row=ok_rows, col=seq_len(nc), color=col_grp)))
      }
      if (length(conflict_grp) > 0)
        hl <- c(hl, list(list(row=conflict_grp, col=seq_len(nc), color="#fde8e8")))

      # pre-compute the shared key/val cols for listed obs vars
      listed_key_col <- NULL; listed_val_col <- NULL
      for (.lv in v) {
        if (.lv$type == "obs" && isTRUE(.lv$props$listed == "yes")) {
          listed_key_col <- .lv$props$key_col
          listed_val_col <- .lv$props$val_col
          break
        }
      }
      is_listed_table <- !is.null(listed_key_col)

      # header rows (phase 3 setting; default 1)
      hdr_rows  <- 1L

      # listed obs: paint shared columns once, not once per variable
      # header row of key column is a meaningless placeholder -- leave it uncoloured
      # also exclude any rows claimed by wide ID vars (they hold ID values, not obs data)
      wide_id_rows <- unlist(lapply(v, function(x)
        if (x$type == "id" && isTRUE(x$props$wide == "yes")) x$props$rows %||% integer(0)
        else integer(0)))
      data_rows <- setdiff(seq_len(nr), c(hdr_rows, wide_id_rows))
      if (is_listed_table) {
        if (!is.null(listed_val_col))
          hl <- c(hl, list(list(row=data_rows, col=listed_val_col, color=.SB$obs)))
        if (!is.null(listed_key_col))
          hl <- c(hl, list(list(row=data_rows, col=listed_key_col, color=.SB$obs_key)))
      }

      for (var in v) {
        p     <- var$props
        if (var$type == "obs" && is_listed_table) next  # already handled above
        if (var$type == "id" && isTRUE(p$wide == "yes")) next  # second pass below

        color     <- if (isTRUE(p$cluster_id == "yes") || isTRUE(p$cluster_obs == "yes")) .SB$cluster
                     else if (var$type == "id") .SB$id
                     else .SB$obs
        color_key <- if (isTRUE(p$cluster_id == "yes") || isTRUE(p$cluster_obs == "yes")) .SB$cluster
                     else if (var$type == "id") .SB$id_key
                     else .SB$obs_key

        cols <- p$columns %||% integer(0)
        if (length(cols) > 0) {
          if (isTRUE(p$cluster_obs == "yes")) {
            # first cluster-obs var: data rows within each block in orange (origin cells
            # are overwritten green by the origins loop below)
            origs <- p$origins %||% list()
            tops  <- sort(vapply(origs, `[[`, integer(1), "row"))
            for (bi in seq_along(tops)) {
              blk_top  <- tops[bi]
              blk_end  <- if (bi < length(tops)) tops[bi + 1L] - 1L else nr
              data_rows_blk <- seq(blk_top + 1L, blk_end)
              if (length(data_rows_blk) > 0)
                hl <- c(hl, list(list(row=data_rows_blk, col=cols, color=.SB$obs)))
            }
          } else if (!is.null(p$cluster_value)) {
            # inherited cluster-obs var: paint only its own block's data rows in orange
            clust_origins <- list()
            for (.cv in v)
              if (isTRUE(.cv$props$cluster_obs == "yes")) {
                clust_origins <- .cv$props$origins %||% list(); break
              }
            tops <- sort(vapply(clust_origins, `[[`, integer(1), "row"))
            bi   <- as.integer(p$cluster_value)
            if (bi >= 1L && bi <= length(tops)) {
              blk_top  <- tops[bi]
              blk_end  <- if (bi < length(tops)) tops[bi + 1L] - 1L else nr
              data_rows_blk <- seq(blk_top + 1L, blk_end)
              if (length(data_rows_blk) > 0)
                hl <- c(hl, list(list(row=data_rows_blk, col=cols, color=.SB$obs)))
            }
          } else {
            has_top   <- isTRUE(p$top == "yes") && length(p$rows %||% integer(0)) > 0
            name_rows <- if (has_top) p$rows else hdr_rows
            data_rows_var <- if (has_top) seq(max(name_rows) + 1L, nr) else seq_len(nr)
            hl <- c(hl, list(list(row=data_rows_var, col=cols, color=color)))
            hl <- c(hl, list(list(row=name_rows,     col=cols, color=color_key)))
          }
        }

        # cluster origins (paints on top of everything -- always green, single cell)
        for (o in p$origins %||% list())
          hl <- c(hl, list(list(row=o$row, col=o$col, color=.SB$cluster)))
      }

      # second pass: wide ID vars -- painted after obs so they are not overwritten
      # their p$rows are values (e.g. "soybean", "maize"), so use light colour, not dark
      for (var in v) {
        p <- var$props
        if (!(var$type == "id" && isTRUE(p$wide == "yes"))) next
        cols      <- p$columns %||% integer(0)
        val_rows  <- p$rows    %||% integer(0)
        if (length(cols) > 0 && length(val_rows) > 0)
          hl <- c(hl, list(list(row=val_rows, col=cols, color=.SB$id)))
      }

      # Phase 3: filter highlights
      for (fi in seq_along(rv$filters)) {
        flt <- rv$filters[[fi]]
        # invert is read from live input to avoid rv write -> re-render -> resize
        inv <- isTRUE(input[[paste0("flt_invert_", fi)]])

        # Finder mode -- evaluate regex against the table and highlight matches
        if (isTRUE(flt$use_find) && nchar(flt$find_pattern) > 0) {
          pat_ok <- tryCatch({ grepl(flt$find_pattern, ""); TRUE }, error = function(e) FALSE)
          if (pat_ok) {
            dim_num    <- suppressWarnings(as.integer(flt$find_dim))
            search_in  <- flt$find_search_in %||% "row"
            sel        <- flt$find_select    %||% "rows"
            tbl_chr    <- as.data.frame(lapply(rv$tbl, as.character), stringsAsFactors = FALSE)
            if (search_in == "row") {
              # Pattern is matched against values within a specific row (or all rows)
              search_rows <- if (!is.na(dim_num) && dim_num >= 1 && dim_num <= nr) dim_num else seq_len(nr)
              # Find which COLUMNS have a match in those rows
              matched_cols <- which(vapply(seq_len(nc), function(cl) {
                any(grepl(flt$find_pattern, as.character(tbl_chr[search_rows, cl])))
              }, logical(1)))
              if (sel == "columns") {
                tint_cols <- if (inv) setdiff(seq_len(nc), matched_cols) else matched_cols
                if (length(tint_cols) > 0)
                  hl <- c(hl, list(list(row=seq_len(nr), col=tint_cols, color="#fff3cd")))
              } else {
                # select = "rows": highlight matched columns as a preview of what was found,
                # but shade the rows that would be selected (all rows matching any pattern hit)
                matched_rows <- which(vapply(seq_len(nr), function(r) {
                  any(grepl(flt$find_pattern, as.character(tbl_chr[r, seq_len(nc)])))
                }, logical(1)))
                tint_rows <- if (inv) setdiff(seq_len(nr), matched_rows) else matched_rows
                if (length(tint_rows) > 0)
                  hl <- c(hl, list(list(row=tint_rows, col=seq_len(nc), color="#fff3cd")))
              }
            } else {
              # Pattern is matched against values within a specific column (or all columns)
              search_cols <- if (!is.na(dim_num) && dim_num >= 1 && dim_num <= nc) dim_num else seq_len(nc)
              matched_rows <- which(vapply(seq_len(nr), function(r) {
                any(grepl(flt$find_pattern, as.character(tbl_chr[r, search_cols])))
              }, logical(1)))
              if (sel == "rows") {
                tint_rows <- if (inv) setdiff(seq_len(nr), matched_rows) else matched_rows
                if (length(tint_rows) > 0)
                  hl <- c(hl, list(list(row=tint_rows, col=seq_len(nc), color="#fff3cd")))
              } else {
                # select = "columns": find which columns have a match
                matched_cols <- which(vapply(seq_len(nc), function(cl) {
                  any(grepl(flt$find_pattern, as.character(tbl_chr[seq_len(nr), cl])))
                }, logical(1)))
                tint_cols <- if (inv) setdiff(seq_len(nc), matched_cols) else matched_cols
                if (length(tint_cols) > 0)
                  hl <- c(hl, list(list(row=seq_len(nr), col=tint_cols, color="#fff3cd")))
              }
            }
          }
        }

        # Click mode -- highlight selected rows/cols (red; inverted = complement)
        if (!isTRUE(flt$use_find)) {
          if (length(flt$rows) > 0) {
            tint_rows <- if (inv) setdiff(seq_len(nr), flt$rows) else flt$rows
            if (length(tint_rows) > 0)
              hl <- c(hl, list(list(row=tint_rows, col=seq_len(nc), color="#fde8e8")))
          }
          if (length(flt$cols) > 0) {
            tint_cols <- if (inv) setdiff(seq_len(nc), flt$cols) else flt$cols
            if (length(tint_cols) > 0)
              hl <- c(hl, list(list(row=seq_len(nr), col=tint_cols, color="#fde8e8")))
          }
        }
      }

      .sb_grid(rv$tbl, hl)
    })

    # -- group function syntax feedback (one output per slot) ----------------
    .grp_fn_feedback <- function(expr_str) {
      if (nchar(trimws(expr_str)) == 0) return(NULL)
      err <- tryCatch({ parse(text = sprintf("function(.x) %s", expr_str)); NULL },
                      error = function(e) conditionMessage(e))
      if (is.null(err))
        shiny::tags$p(style="font-size:11px;color:#2d6a16;margin-top:2px;", "\u2713 Valid expression")
      else
        shiny::tags$p(style="font-size:11px;color:#c0392b;margin-top:2px;", paste("Syntax error:", err))
    }
    for (.n in seq_len(10)) {
      local({
        n <- .n
        output[[paste0("sb_grp_char_fb_", n)]] <- shiny::renderUI(
          .grp_fn_feedback(input[[paste0("grp_char_fn_live_", n)]] %||% ""))
        output[[paste0("sb_grp_num_fb_",  n)]] <- shiny::renderUI(
          .grp_fn_feedback(input[[paste0("grp_num_fn_live_",  n)]] %||% ""))
      })
    }

    output$sb_split_regex_feedback <- shiny::renderUI({
      regex <- input$sb_split_regex_live %||% ""
      if (nchar(regex) == 0) return(NULL)
      n_groups <- tryCatch(
        length(regmatches(regex, gregexpr("(?<!\\\\)\\((?!\\?[:=!<])", regex, perl=TRUE))[[1]]),
        error = function(e) 0L)
      if (n_groups >= 1)
        shiny::tags$p(style="font-size:11px;color:#2d6a16;margin-top:2px;",
          paste0("\u2713 ", n_groups, " capture group(s) found"))
      else
        shiny::tags$p(style="font-size:11px;color:#c0392b;margin-top:2px;",
          "No capture groups found \u2014 wrap each part to extract in parentheses, e.g. ([^_]+)_(.*)")
    })

    output$sb_legend <- shiny::renderUI({
      has_listed      <- any(vapply(rv$vars, function(x)
        x$type == "obs" && isTRUE(x$props$listed == "yes"), logical(1)))
      is_cluster      <- any(vapply(rv$vars, function(x)
        isTRUE(x$props$clustered == "yes"), logical(1)))
      is_clust_obs    <- any(vapply(rv$vars, function(x)
        x$type == "obs" && isTRUE(x$props$cluster_obs == "yes"), logical(1)))
      shiny::tags$div(class = "sb-legend",
        .sb_swatch(.SB$id_key, "ID variable (name)"),
        .sb_swatch(.SB$id,     "ID variable (values)"),
        if (has_listed)
          .sb_swatch(.SB$obs_key, "Obs variable (key col)")
        else
          .sb_swatch(.SB$obs_key, "Obs variable (name)"),
        .sb_swatch(.SB$obs,     "Obs variable (values)"),
        if (is_cluster || is_clust_obs)
          .sb_swatch(.SB$cluster,
            if (is_clust_obs) "Cluster origin (obs variable)" else "Cluster origin"),
        .sb_swatch(.SB$header,  "Header row")
      )
    })

    # -- schema code ---------------------------------------------------------
    output$sb_code <- shiny::renderText({ .sb_gen_code(rv, input) })

    # -- misc ----------------------------------------------------------------
    output$sb_has_table <- shiny::renderText({
      if (is.null(rv$tbl)) "no" else "yes"
    })
    shiny::outputOptions(output, "sb_has_table", suspendWhenHidden = FALSE)

    output$sb_dl <- shiny::downloadHandler(
      filename = "schema.rds",
      content  = function(file) {
        code <- .sb_gen_code(rv, input)
        sc <- tryCatch(eval(parse(text=code), envir=new.env(parent=asNamespace("tabshiftr"))),
                       error = function(e) NULL)
        if (!is.null(sc)) saveRDS(sc, file) else writeLines(code, file)
      }
    )

    # -- Finish: evaluate schema, store in result_env, close app ---------------
    shiny::observeEvent(input$sb_finish, {
      code <- .sb_gen_code(rv, input)
      sc <- tryCatch(eval(parse(text=code), envir=new.env(parent=asNamespace("tabshiftr"))),
                     error = function(e) NULL)
      result_env$schema <- sc
      shiny::stopApp()
    })

    # -- Reorganise: run reorganise() and show the result ----------------------
    rv_reorg <- shiny::reactiveValues(
      schema_error = NULL,   # error evaluating schema code
      id_vars      = NULL,   # list-per-cluster from getIDVars()
      id_error     = NULL,
      obs_vars     = NULL,   # list-per-cluster from getObsVars()
      obs_error    = NULL,
      result       = NULL,   # final tidy tibble from reorganise()
      reorg_error  = NULL
    )

    shiny::observeEvent(input$sb_reorganise, {
      shiny::req(rv$tbl)
      code <- .sb_gen_code(rv, input)

      # 1. evaluate schema code
      sc <- tryCatch(eval(parse(text=code), envir=new.env(parent=asNamespace("tabshiftr"))),
                     error = function(e) e)
      if (inherits(sc, "error")) {
        rv_reorg$schema_error <- conditionMessage(sc)
        rv_reorg$id_vars      <- NULL
        rv_reorg$id_error     <- NULL
        rv_reorg$obs_vars     <- NULL
        rv_reorg$obs_error    <- NULL
        rv_reorg$result       <- NULL
        rv_reorg$reorg_error  <- NULL
        return()
      }
      rv_reorg$schema_error <- NULL

      # validate schema against input (required before get*Vars)
      vsc <- tryCatch(validateSchema(input = rv$tbl, schema = sc),
                      error = function(e) e)
      if (inherits(vsc, "error")) {
        rv_reorg$schema_error <- conditionMessage(vsc)
        rv_reorg$id_vars      <- NULL
        rv_reorg$obs_vars     <- NULL
        rv_reorg$result       <- NULL
        rv_reorg$reorg_error  <- NULL
        return()
      }

      # validate input (header splice + row groups)
      vinput <- tryCatch(validateInput(schema = vsc, input = rv$tbl), error = function(e) e)
      if (inherits(vinput, "error")) {
        rv_reorg$schema_error <- conditionMessage(vinput)
        rv_reorg$id_vars      <- NULL
        rv_reorg$obs_vars     <- NULL
        rv_reorg$result       <- NULL
        rv_reorg$reorg_error  <- NULL
        return()
      }

      # 2. extract ID vars
      id <- tryCatch(getIDVars(schema = vsc, input = vinput), error = function(e) e)
      if (inherits(id, "error")) {
        rv_reorg$id_vars  <- NULL
        rv_reorg$id_error <- conditionMessage(id)
      } else {
        rv_reorg$id_vars  <- id
        rv_reorg$id_error <- NULL
      }

      # 3. extract obs vars
      obs <- tryCatch(getObsVars(schema = vsc, input = vinput), error = function(e) e)
      if (inherits(obs, "error")) {
        rv_reorg$obs_vars  <- NULL
        rv_reorg$obs_error <- conditionMessage(obs)
      } else {
        rv_reorg$obs_vars  <- obs
        rv_reorg$obs_error <- NULL
      }

      # 4. full reorganise
      out <- tryCatch(reorganise(rv$tbl, schema = sc), error = function(e) e)
      if (inherits(out, "error")) {
        rv_reorg$result      <- NULL
        rv_reorg$reorg_error <- conditionMessage(out)
      } else {
        rv_reorg$result      <- if (nrow(out) == 0 || ncol(out) == 0) NULL else out
        rv_reorg$reorg_error <- NULL
      }
    })

    # helper: render one cluster's worth of a get*Vars list as a plain HTML table
    .sb_cluster_tbl <- function(cluster_list) {
      if (is.null(cluster_list) || length(cluster_list) == 0) return(NULL)
      tbl <- tryCatch({
        parts <- lapply(names(cluster_list), function(nm) {
          x <- cluster_list[[nm]]
          if (!is.data.frame(x)) x <- as.data.frame(x)
          # multi-column elements (e.g. listed obs: key + value) -- keep all columns
          if (ncol(x) > 1) x else stats::setNames(x[, 1, drop = FALSE], nm)
        })
        do.call(cbind, parts)
      }, error = function(e) NULL)
      if (is.null(tbl) || ncol(tbl) == 0) return(NULL)
      # render as plain HTML table (works inside renderUI without DT binding issues)
      col_heads <- paste(sprintf("<th>%s</th>", colnames(tbl)), collapse = "")
      rows_html <- paste(vapply(seq_len(nrow(tbl)), function(r) {
        cells <- paste(sprintf("<td>%s</td>", as.character(unlist(tbl[r, ]))), collapse = "")
        paste0("<tr>", cells, "</tr>")
      }, character(1)), collapse = "")
      shiny::HTML(sprintf(
        "<div style='overflow-x:auto;max-height:200px;overflow-y:auto;'>
         <table style='border-collapse:collapse;font-size:12px;width:100%%;'>
           <thead><tr style='background:#f0f0f0;'>%s</tr></thead>
           <tbody>%s</tbody>
         </table></div>",
        col_heads, rows_html))
    }

    # helper: render a plain data frame as an HTML table
    .sb_df_tbl <- function(df) {
      if (is.null(df) || ncol(df) == 0) return(NULL)
      col_heads <- paste(sprintf("<th>%s</th>", colnames(df)), collapse = "")
      rows_html <- paste(vapply(seq_len(nrow(df)), function(r) {
        cells <- paste(sprintf("<td>%s</td>", as.character(unlist(df[r, ]))), collapse = "")
        paste0("<tr>", cells, "</tr>")
      }, character(1)), collapse = "")
      shiny::HTML(sprintf(
        "<div style='overflow-x:auto;max-height:260px;overflow-y:auto;'>
         <table style='border-collapse:collapse;font-size:12px;width:100%%;'>
           <thead><tr style='background:#f0f0f0;'>%s</tr></thead>
           <tbody>%s</tbody>
         </table></div>",
        col_heads, rows_html))
    }

    output$sb_reorganise_out <- shiny::renderUI({
      nothing <- is.null(rv_reorg$schema_error) &&
                 is.null(rv_reorg$id_vars)      && is.null(rv_reorg$id_error) &&
                 is.null(rv_reorg$obs_vars)     && is.null(rv_reorg$obs_error) &&
                 is.null(rv_reorg$result)        && is.null(rv_reorg$reorg_error)
      if (nothing) return(NULL)

      # build tab headers and panels
      tab_bar <- shiny::tags$div(class = "sb-tabs",
        shiny::tags$button(class = "sb-tab active",
          `data-tabgroup` = "reorg", `data-tabid` = "id", "ID variables"),
        shiny::tags$button(class = "sb-tab",
          `data-tabgroup` = "reorg", `data-tabid` = "obs", "Observed variables"),
        shiny::tags$button(class = "sb-tab",
          `data-tabgroup` = "reorg", `data-tabid` = "out", "Output")
      )

      id_content <- if (!is.null(rv_reorg$schema_error))
        shiny::tags$p(style="color:#c0392b;font-size:12px;",
          paste0("Schema error: ", rv_reorg$schema_error))
      else if (!is.null(rv_reorg$id_error))
        shiny::tags$p(style="color:#c0392b;font-size:12px;", rv_reorg$id_error)
      else if (is.null(rv_reorg$id_vars))
        shiny::tags$p(style="color:#888;font-size:12px;", "None declared.")
      else
        shiny::tagList(lapply(seq_along(rv_reorg$id_vars), function(i) {
          shiny::tags$div(
            if (length(rv_reorg$id_vars) > 1)
              shiny::tags$p(style="font-size:11px;color:#555;margin:4px 0 2px;",
                paste0("Cluster ", i)),
            .sb_cluster_tbl(rv_reorg$id_vars[[i]])
          )
        }))

      obs_content <- if (!is.null(rv_reorg$schema_error))
        shiny::tags$p(style="color:#c0392b;font-size:12px;",
          paste0("Schema error: ", rv_reorg$schema_error))
      else if (!is.null(rv_reorg$obs_error))
        shiny::tags$p(style="color:#c0392b;font-size:12px;", rv_reorg$obs_error)
      else if (is.null(rv_reorg$obs_vars))
        shiny::tags$p(style="color:#888;font-size:12px;", "None declared.")
      else
        shiny::tagList(lapply(seq_along(rv_reorg$obs_vars), function(i) {
          shiny::tags$div(
            if (length(rv_reorg$obs_vars) > 1)
              shiny::tags$p(style="font-size:11px;color:#555;margin:4px 0 2px;",
                paste0("Cluster ", i)),
            .sb_cluster_tbl(rv_reorg$obs_vars[[i]])
          )
        }))

      out_content <- if (!is.null(rv_reorg$reorg_error))
        shiny::tags$p(style="color:#c0392b;font-size:12px;", rv_reorg$reorg_error)
      else if (is.null(rv_reorg$result))
        shiny::tags$p(style="color:#888;font-size:12px;",
          "Add at least one observed variable to see the final output.")
      else
        .sb_df_tbl(rv_reorg$result)

      shiny::tags$div(class = "sb-panel",
        shiny::tags$h4("Reorganise debug"),
        tab_bar,
        shiny::tags$div(class = "sb-tab-panel active",
          `data-tabgroup` = "reorg", `data-tabid` = "id",  id_content),
        shiny::tags$div(class = "sb-tab-panel",
          `data-tabgroup` = "reorg", `data-tabid` = "obs", obs_content),
        shiny::tags$div(class = "sb-tab-panel",
          `data-tabgroup` = "reorg", `data-tabid` = "out", out_content)
      )
    })


  }

  shiny::shinyApp(ui = ui, server = server)
}

# ---------------------------------------------------------------------------
# Phase 1 question UI -- returns tagList of sequential yes/no questions
# ---------------------------------------------------------------------------

.sb_phase1_questions <- function(rv) {
  C <- function(g, v, thumb, lbl) shiny::HTML(.sb_card(g, v, thumb, lbl))

  qs <- shiny::tagList()

  # Q1: multiple header rows?
  qs <- shiny::tagList(qs,
    shiny::tags$div(class = "sb-q",
      shiny::tags$div(class = "sb-q-text",
        "Q1 \u2014 Does the table have more than one header row?"),
      shiny::tags$div(class = "sb-hint",
        "Multiple header rows arise when a wide ID variable pushes obs variable names into a second row."),
      shiny::tags$div(class = "sb-cards",
        C("headers", "one",   .sb_thumbs$tidy,    "One header row"),
        C("headers", "multi", .sb_thumbs$wide_id, "Multiple header rows")
      )
    )
  )
  if (is.null(rv$q_headers)) return(qs)

  # Q2: listed obs?
  qs <- shiny::tagList(qs,
    shiny::tags$div(class = "sb-q",
      shiny::tags$div(class = "sb-q-text",
        "Q2 \u2014 Is there a key-value column pair?"),
      shiny::tags$div(class = "sb-hint",
        "One column with a small repeating set of strings (obs variable names), paired with one column of numbers."),
      shiny::tags$div(class = "sb-cards",
        C("listed", "no",  .sb_thumbs$tidy,   "No key-value pair"),
        C("listed", "yes", .sb_thumbs$listed, "Key-value pair present")
      )
    )
  )

  qs
}

# ---------------------------------------------------------------------------
# Derive table type string from phase 1 answers
# ---------------------------------------------------------------------------

.sb_derive_type <- function(headers, listed) {
  if (is.null(headers)) return(NULL)
  if (is.null(listed))  return(NULL)
  wide <- headers == "multi"
  lst  <- listed  == "yes"
  if (wide && lst) return("wide_listed")
  if (wide)        return("wide_id")
  if (lst)         return("listed")
  return("tidy")
}

.sb_type_label <- function(type) {
  switch(type,
    tidy         = "Tidy / near-tidy table",
    listed       = "Listed observed variables",
    wide_id      = "Wide identifying variable(s)",
    wide_listed  = "Wide ID variable(s) + listed observed variables",
    implicit     = "Implicit cluster ID",
    obs_clusters = "Clusters of observed variables",
    nested       = "Nested clusters",
    horizontal   = "Horizontal clusters",
    vertical     = "Vertical clusters",
    messy        = "Messy (irregular) clusters",
    type)
}

# ---------------------------------------------------------------------------
# Mini-tree for a single variable  (called from output$sb_phase2 renderUI)
# type = "id" | "obs"
# step = integer, which question we're on (1-based)
# p    = props list (accumulated answers)
# ---------------------------------------------------------------------------

# Build the ordered step list for a variable.
.sb_steps <- function(type, p = list(), current_step = NULL,
                      listed_answered = FALSE, listed_answer = NULL,
                      cluster_obs_answer = NULL,
                      n_other_obs = 0L, n_clusters = 1L) {
  if (type == "id") {
    # value not in table -- collapse to name + implicit question + explicit value
    if (isTRUE(p$implicit == "no")) {
      return(list(
        list(q    = "What is this variable's name?",
             hint = NULL,
             ui   = "text_name",
             key  = "name"),
        list(q    = "Is this variable's value present in the table in some form?",
             hint = "If no, the value comes from context (filename, sheet name, or a known constant) and you will supply it directly.",
             ui   = "yn",
             key  = "implicit"),
        list(q    = "What value does this variable take?",
             hint = "Supply the constant directly (e.g. the filename, sheet name, or a known value).",
             ui   = "text_value",
             key  = "value")
      ))
    }
    base <- list(
      list(q    = "What is this variable's name?",
           hint = NULL,
           ui   = "text_name",
           key  = "name"),
      list(q    = "Is this variable's value present in the table in some form?",
           hint = "If no, the value comes from context (filename, sheet name, or a known constant) and you will supply it directly.",
           ui   = "yn",
           key  = "implicit"),
      list(q    = "Does this variable identify separate data blocks (clusters), with one distinct value per block?",
           hint = "Clusters are repeated sections of the table with the same column structure. Each block has a distinct value of this variable (e.g. a territory name above each block). If yes, you will mark the top-left cell of each block.",
           ui   = "yn_then_origins",
           key  = "clustered"),
      list(q    = "Is this variable only present in some clusters (distinct)?",
           hint = "Mark distinct if the variable appears in only one cluster but its value applies to all -- e.g. a year range shown once for the whole table.",
           ui   = "yn",
           key  = "distinct"),
      list(q    = "Which column(s) contain this variable's values?",
           hint = "Click one or more columns in the table on the right.",
           ui   = "col_select",
           key  = "columns"),
      list(q    = "Is this variable tidy in that column?",
           hint = "Tidy means: one value per cell, no merging needed, not spread across column headers, not packed together with another variable.",
           ui   = "yn",
           key  = "tidy"),
      list(q    = "Are values spread across column headers instead of a single column (wide)?",
           hint = "A wide ID has its values as column names -- including cluster ID variables, where the cluster name appears as a header above each block. Click the row containing those values.",
           ui   = "yn_then_row",
           key  = "wide"),
      list(q    = "Is this variable spread across several columns that need to be merged?",
           hint = "Select the columns above if you have not already, then enter the character to merge them with (e.g. a space or hyphen).",
           ui   = "yn_then_text_merge",
           key  = "merge_cols"),
      list(q    = "Are two variables packed together in one column (needs splitting)?",
           hint = "Select the column above if you have not already, then enter the regex to split by.",
           ui   = "yn_then_text_split",
           key  = "split")
    )
    n_cols    <- length(p$columns %||% integer(0))
    n_origins <- length(p$origins %||% list())
    # for the clustered variable itself, use its own origin count (stable once past col_select)
    # for other variables, use n_clusters passed in from the table-level clustered variable
    effective_clusters <- if (n_origins > 0) n_origins else n_clusters
    n_cols_per_cluster <- if (effective_clusters > 1) ceiling(n_cols / effective_clusters) else n_cols

    # skip tidy only when >1 col per cluster AND tidy not yet answered
    # for the cluster ID var itself (has origins), use n_cols directly -- dividing by n_origins
    # would cause the tidy step to reappear mid-flow as origins accumulate, shifting all indices
    tidy_cols <- if (n_origins > 0) n_cols else n_cols_per_cluster
    if (is.null(p$tidy) && tidy_cols > 1) {
      base <- Filter(function(s) s$key != "tidy", base)
    }

    # drop wide/merge/split steps when tidy (only applies when single column)
    if (isTRUE(p$tidy == "yes")) {
      base <- Filter(function(s) !s$key %in% c("wide", "merge_cols", "split"), base)
    }

    # wide ID: values are column headers -- merge/split don't apply
    if (isTRUE(p$wide == "yes")) {
      base <- Filter(function(s) !s$key %in% c("merge_cols", "split"), base)
    }

    # merge and split are mutually exclusive; only drop the later one
    if (isTRUE(p$merge_cols == "yes")) {
      base <- Filter(function(s) s$key != "split", base)
    }

    # remove distinct when not in a clustered table or when this var IS the cluster ID
    if (n_clusters <= 1 || isTRUE(p$clustered == "yes")) {
      base <- Filter(function(s) s$key != "distinct", base)
    }

    # distinct variable: replace tidy/wide/merge/split with a direct row-select step
    if (isTRUE(p$distinct == "yes")) {
      base <- Filter(function(s) !s$key %in% c("tidy", "wide", "merge_cols", "split"), base)
      base <- c(base, list(
        list(q    = "Which row(s) contain this variable's values?",
             hint = "Click the row(s) in the table where this variable's value appears.",
             ui   = "row_select",
             key  = "rows")
      ))
    }

    # when clustered: append origin_select + nested + distinct
    if (isTRUE(p$clustered == "yes")) {
      cluster_steps <- list(
        list(q    = "Click the top-left cell of each data block in the table.",
             hint = "The top-left cell is where a block's first data row meets its leftmost column. Its row gives 'top =', its column gives 'left ='. Click again to deselect.",
             ui   = "origin_select",
             key  = "origins"),
        list(q    = "Are the blocks grouped into a higher-level variable (nested)?",
             hint = "Nested clusters have an outer grouping variable that spans multiple inner blocks -- e.g. regions containing country-level clusters.",
             ui   = "yn",
             key  = "nested")
      )

      if (isTRUE(p$nested == "yes")) {
        cluster_steps <- c(cluster_steps, list(
          list(q    = "What is the name of the grouping (outer) variable?",
               hint = "This is the variable that spans multiple inner blocks, e.g. 'territories' or 'regions'.",
               ui   = "text_name",
               key  = "group_name"),
          list(q    = "Click the cell where each group label appears in the table.",
               hint = "One click per group -- the row of each click gives the position of that group's label.",
               ui   = "group_origin_select",
               key  = "group_origins"),
          list(q    = "Assign each cluster to a group.",
               hint = "For each inner block (in the order you clicked them), enter which group number it belongs to.",
               ui   = "member_assign",
               key  = "member")
        ))
      }

      # wide variable is the cluster ID itself -- nesting doesn't apply
      if (isTRUE(p$wide == "yes"))
        cluster_steps <- Filter(function(s) s$key == "origins", cluster_steps)
      base <- c(base, cluster_steps)
    }
    base
  } else {
    # --- cluster-obs table: observed variables ARE the cluster structure ---
    # Inherited branch: another obs var already answered cluster_obs == "yes".
    # This var skips straight to: name -> cluster number -> columns.
    if (isTRUE(cluster_obs_answer == "yes")) {
      return(list(
        list(q    = "What is this variable's name?",
             hint = NULL,
             ui   = "text_name",
             key  = "name"),
        list(q    = "Which cluster number does this variable belong to?",
             hint = sprintf("Enter the running index of the block this variable lives in (1 = first block, 2 = second, \u2026 up to %d).", n_clusters),
             ui   = "cluster_num_select",
             key  = "cluster_value"),
        list(q    = "Which column(s) contain this variable's values within its cluster?",
             hint = "Click one or more columns in the table on the right.",
             ui   = "col_select",
             key  = "columns")
      ))
    }

    # First obs var that answered cluster_obs == "yes" (full cluster-obs setup flow).
    if (isTRUE(p$cluster_obs == "yes")) {
      return(list(
        list(q    = "Does this variable identify separate data blocks (clusters), with one distinct value per block?",
             hint = "In a clusters-of-observed-variables table every observed variable belongs to exactly one block. Answer Yes to set up the cluster structure, then describe each variable individually.",
             ui   = "yn_then_origins",
             key  = "cluster_obs"),
        list(q    = "Click the top-left cell of each data block in the table.",
             hint = "The top-left cell is where a block's first data row meets its leftmost column. Its row gives 'top =', its column gives 'left ='. Click again to deselect.",
             ui   = "origin_select",
             key  = "origins"),
        list(q    = "What is this variable's name?",
             hint = NULL,
             ui   = "text_name",
             key  = "name"),
        list(q    = "Which cluster number does this variable belong to?",
             hint = sprintf("Enter the running index of the block this variable lives in (1 = first block, \u2026 up to %d).", n_clusters),
             ui   = "cluster_num_select",
             key  = "cluster_value"),
        list(q    = "Which column(s) contain this variable's values within its cluster?",
             hint = "Click one or more columns in the table on the right.",
             ui   = "col_select",
             key  = "columns")
      ))
    }

    # --- normal (non-cluster-obs) obs variable ---
    is_listed <- isTRUE(p$listed == "yes") || isTRUE(listed_answer == "yes")

    cluster_obs_q <- list(
      list(q    = "Does this variable identify separate data blocks (clusters), with one distinct value per block?",
           hint = "In a clusters-of-observed-variables table each observed variable belongs to exactly one block. Answer No if this is a regular measured value in a non-clustered or ID-clustered table.",
           ui   = "yn",
           key  = "cluster_obs")
    )

    obs_tail <- list(
      list(q    = "What is this variable's name?",
           hint = NULL,
           ui   = "text_name",
           key  = "name")
    )

    if (is_listed) {
      # listed flow: name -> key value -> done (key/val cols already captured on yn_then_kv)
      obs_tail <- c(obs_tail, list(
        list(q    = "Which value in the key column identifies this variable?",
             hint = "Select the value that appears in the key column for this variable's rows.",
             ui   = "key_value_select",
             key  = "listed_value")
      ))
    } else {
      # normal flow
      obs_tail <- c(obs_tail, list(
        list(q    = "Is the name of this variable in a header row nested under a wide ID?",
             hint = "If yes, click the row that contains the obs variable name.",
             ui   = "yn_then_row",
             key  = "top"),
        list(q    = "Which column(s) contain this variable's values?",
             hint = "Click one or more columns in the table on the right.",
             ui   = "col_select",
             key  = "columns"),
        list(q    = "Conversion factor (leave as 1 if none needed):",
             hint = "Values in the column will be multiplied by this number. Use e.g. 1000 to convert tonnes to kg, or 0.001 for the reverse.",
             ui   = "num_factor",
             key  = "factor")
      ))
    }

    # build full step list: cluster_obs first (only if not yet answered by another var),
    # then listed (first obs only), then rest
    obs_base <- if (is.null(cluster_obs_answer)) cluster_obs_q else list()
    if (n_other_obs == 0L)
      obs_base <- c(obs_base, list(
        list(q    = "Are ALL observed variables listed as a key-value pair (names stacked in one column, values in another)?",
             hint = "If yes, click the key column (names) and the value column separately.",
             ui   = "yn_then_kv",
             key  = "listed")))
    obs_base <- c(obs_base, obs_tail)
    obs_base
  }
}

.sb_var_mini_tree <- function(type, step, p, listed_answered = FALSE, tbl = NULL, listed_key_col = NULL, listed_answer = NULL,
                              cluster_obs_answer = NULL, cluster_obs_origins = list(),
                              n_other_obs = 0L, n_clusters = 1L) {
  steps   <- .sb_steps(type, p, step, listed_answered, listed_answer = listed_answer,
                       cluster_obs_answer = cluster_obs_answer,
                       n_other_obs = n_other_obs, n_clusters = n_clusters)
  n_steps <- length(steps)
  step    <- max(1L, min(step, n_steps))

  # Only show green/cluster-ID styling once the user is ON or PAST the origin_select step
  origin_step_idx <- which(vapply(steps, function(s) s$ui == "origin_select", logical(1)))
  is_cluster_id   <- isTRUE(p$cluster_id == "yes") &&
                     length(origin_step_idx) > 0 &&
                     step >= origin_step_idx[1]
  is_cluster_obs  <- isTRUE(p$cluster_obs == "yes") &&
                     length(origin_step_idx) > 0 &&
                     step >= origin_step_idx[1]

  bg  <- if (is_cluster_id || is_cluster_obs) "#f0fbe8"
         else if (type == "id") "#f0f6fd" else "#fef0e6"
  bdr <- if (is_cluster_id || is_cluster_obs) "#70AD47"
         else if (type == "id") "#5B9BD5"  else "#ED7D31"
  lbl <- if (is_cluster_id) "ID variable (cluster ID)"
         else if (is_cluster_obs) "Observed variable (cluster ID)"
         else if (type == "id") "ID variable"
         else "Observed variable"
  s       <- steps[[step]]

  origins_str <- {
    origs <- p$origins %||% list()
    if (length(origs) == 0) "none \u2014 click top-left cell of each cluster"
    else paste(vapply(origs, function(o) paste0("(r", o$row, ",c", o$col, ")"),
                      character(1)), collapse = ", ")
  }

  q_widget <- switch(s$ui,
    text_name = shiny::textInput("sb_var_name", NULL,
      value       = p$name %||% "",
      placeholder = if (type == "id") "e.g. territory, year" else "e.g. area, production",
      width       = "240px"),

    text_value = shiny::textInput("sb_text_answer", NULL,
      value       = p$value %||% "",
      placeholder = "e.g. 2020, or the filename",
      width       = "240px"),

    yn = shiny::radioButtons("sb_yn_answer", NULL,
      choices  = c("No" = "no", "Yes" = "yes"),
      selected = p[[s$key]] %||% character(0),
      inline   = TRUE),

    row_select = shiny::tags$p(style = "font-size:12px;color:#555;margin-top:6px;",
      paste0("Selected row(s): ",
        if (length(p$rows) > 0) paste(p$rows, collapse = ", ")
        else "none \u2014 click a row number in the table")),

    yn_then_row = shiny::tagList(
      shiny::radioButtons("sb_yn_answer", NULL,
        choices  = c("No" = "no", "Yes" = "yes"),
        selected = p[[s$key]] %||% character(0),
        inline   = TRUE),
      if (isTRUE(p[[s$key]] == "yes"))
        shiny::tags$p(style = "font-size:12px;color:#555;margin-top:6px;",
          paste0("Selected row(s): ",
            if (length(p$rows) > 0) paste(p$rows, collapse = ", ")
            else "none \u2014 click a row number in the table"))
    ),

    yn_then_cols = shiny::tagList(
      shiny::radioButtons("sb_yn_answer", NULL,
        choices  = c("No" = "no", "Yes" = "yes"),
        selected = p[[s$key]] %||% character(0),
        inline   = TRUE),
      if (isTRUE(p[[s$key]] == "yes"))
        shiny::tags$p(style = "font-size:12px;color:#555;margin-top:6px;",
          paste0("Selected columns: ",
            if (length(p$columns) > 0) paste(p$columns, collapse = ", ")
            else "none \u2014 click columns in the table"))
    ),

    yn_then_col1 = shiny::tagList(
      shiny::radioButtons("sb_yn_answer", NULL,
        choices  = c("No" = "no", "Yes" = "yes"),
        selected = p[[s$key]] %||% character(0),
        inline   = TRUE),
      if (isTRUE(p[[s$key]] == "yes"))
        shiny::tags$p(style = "font-size:12px;color:#555;margin-top:6px;",
          paste0("Selected column: ",
            if (length(p$columns) > 0) paste(p$columns, collapse = ", ")
            else "none \u2014 click a column in the table"))
    ),

    yn_then_text_merge = shiny::tagList(
      shiny::radioButtons("sb_yn_answer", NULL,
        choices  = c("No" = "no", "Yes" = "yes"),
        selected = p[[s$key]] %||% character(0),
        inline   = TRUE),
      if (isTRUE(p[[s$key]] == "yes"))
        shiny::textInput("sb_text_answer", "Merge character (type the character itself, not quoted):",
          value       = p$merge_char %||% " ",
          placeholder = "e.g. a space, or -",
          width       = "240px")
    ),

    yn_then_text_split = shiny::tagList(
      shiny::radioButtons("sb_yn_answer", NULL,
        choices  = c("No" = "no", "Yes" = "yes"),
        selected = p[[s$key]] %||% character(0),
        inline   = TRUE),
      if (isTRUE(p[[s$key]] == "yes"))
        shiny::tagList(
          shiny::tags$div(
            shiny::tags$label(style="font-size:12px;", "Split regex (must contain capture groups):"),
            shiny::tags$input(
              id          = "sb_text_answer",
              type        = "text",
              class       = "form-control",
              value       = p$split_regex %||% "",
              placeholder = 'e.g. ([^_]+)_(.*)',
              style       = "width:260px;",
              oninput     = "Shiny.setInputValue('sb_split_regex_live', this.value, {priority:'event'})"
            ),
            shiny::tags$script(sprintf(
              "Shiny.setInputValue('sb_split_regex_live', '%s', {priority:'event'});",
              gsub("'", "\\\\'", p$split_regex %||% "")
            ))
          ),
          shiny::uiOutput("sb_split_regex_feedback")
        )
    ),

    yn_then_origins = shiny::radioButtons("sb_yn_answer", NULL,
      choices  = c("No" = "no", "Yes" = "yes"),
      selected = p[[s$key]] %||% character(0),
      inline   = TRUE),

    yn_then_kv = shiny::tagList(
      shiny::radioButtons("sb_yn_answer", NULL,
        choices  = c("No" = "no", "Yes" = "yes"),
        selected = p[[s$key]] %||% character(0),
        inline   = TRUE),
      if (isTRUE(p[[s$key]] == "yes"))
        shiny::tags$p(style = "font-size:12px;color:#555;margin-top:6px;",
          paste0("Key col: ", p$key_col %||% "none",
                 "  \u2014  Value col(s): ",
                 if (length(p$val_col %||% integer(0)) > 1)
                   paste0("c(", paste(p$val_col, collapse=", "), ")")
                 else if (!is.null(p$val_col)) p$val_col
                 else "none \u2014 click columns in the table"))
    ),

    num_factor = shiny::numericInput("sb_text_answer", NULL,
      value = as.numeric(p$factor %||% "1"),
      step  = 0.001, width = "140px"),

    cluster_num_select = shiny::numericInput("sb_cluster_num", NULL,
      value = as.integer(p$cluster_value %||% 1L),
      min = 1L, max = n_clusters, step = 1L, width = "100px"),

    key_value_select = {
      # derive candidate values from the shared key column
      # skip row 1 -- table is loaded without headers so row 1 is the original header
      key_col <- listed_key_col %||% p$key_col %||% NULL
      choices <- if (!is.null(tbl) && !is.null(key_col) && key_col <= ncol(tbl) && nrow(tbl) > 1) {
        vals <- unique(as.character(tbl[-1, key_col, drop = TRUE]))
        vals <- sort(vals[!is.na(vals) & nchar(vals) > 0])
        vals
      } else character(0)
      if (length(choices) > 0) {
        shiny::selectInput("sb_listed_value", NULL,
          choices  = choices,
          selected = p$listed_value %||% choices[1],
          width    = "220px")
      } else {
        shiny::tagList(
          shiny::tags$p(style="font-size:12px;color:#888;",
            "Key column not yet selected \u2014 go back and click the key column first."),
          shiny::textInput("sb_listed_value", NULL,
            value       = p$listed_value %||% "",
            placeholder = "type the key value manually",
            width       = "220px")
        )
      }
    },

    col_select = shiny::tags$p(style = "font-size:12px;color:#555;",
      paste0("Selected: ",
        if (length(p$columns) > 0) paste(p$columns, collapse = ", ")
        else "none \u2014 click column(s) in the table on the right")),

    origin_select = {
      origs <- p$origins %||% list()
      if (length(origs) == 0) {
        shiny::tags$p(style = "font-size:12px;color:#888;font-style:italic;",
          "No origins marked yet \u2014 click the top-left cell of each cluster in the table.")
      } else {
        shiny::tagList(
          shiny::tags$p(style = "font-size:12px;color:#2d6a16;margin-bottom:4px;",
            paste0(length(origs), " cluster origin(s) marked:")),
          shiny::tags$ul(style = "margin:0;padding-left:18px;font-size:12px;color:#555;",
            lapply(origs, function(o)
              shiny::tags$li(paste0("Row ", o$row, ", column ", o$col))))
        )
      }
    },

    group_origin_select = {
      gorigs <- p$group_origins %||% list()
      if (length(gorigs) == 0) {
        shiny::tags$p(style = "font-size:12px;color:#888;font-style:italic;",
          "No group positions marked yet \u2014 click the cell where each group label appears.")
      } else {
        shiny::tagList(
          shiny::tags$p(style = "font-size:12px;color:#2d6a16;margin-bottom:4px;",
            paste0(length(gorigs), " group position(s) marked:")),
          shiny::tags$ul(style = "margin:0;padding-left:18px;font-size:12px;color:#555;",
            lapply(gorigs, function(o)
              shiny::tags$li(paste0("Row ", o$row, ", column ", o$col))))
        )
      }
    },

    member_assign = {
      origs  <- p$origins %||% list()
      member <- p$member  %||% integer(0)
      n_grps <- length(p$group_origins %||% list())
      if (length(origs) == 0) {
        shiny::tags$p(style="font-size:12px;color:#888;",
          "No cluster origins defined yet \u2014 go back and mark cluster origins first.")
      } else {
        shiny::tagList(
          shiny::tags$p(style="font-size:12px;color:#555;margin-bottom:6px;",
            paste0("Assign each of the ", length(origs), " clusters to a group (1\u2013", max(1L, n_grps), "):"),
          ),
          do.call(shiny::tagList, lapply(seq_along(origs), function(i) {
            shiny::tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:4px;",
              shiny::tags$span(style="font-size:12px;color:#555;min-width:120px;",
                paste0("Cluster ", i, " (row ", origs[[i]]$row, "):  group")),
              shiny::numericInput(paste0("sb_member_", i), NULL,
                value = member[i] %||% 1L,
                min = 1L, max = max(1L, n_grps), step = 1L, width = "70px")
            )
          }))
        )
      }
    }
  )

  prog_dots <- lapply(seq_len(n_steps), function(i) {
    cls <- if (i < step) "sb-dot done"
           else if (i == step) "sb-dot active"
           else "sb-dot"
    shiny::tags$div(class = cls)
  })

  shiny::tags$div(
    style = paste0("margin-top:10px;padding:12px;background:", bg,
                   ";border-radius:6px;border-left:3px solid ", bdr, ";"),
    shiny::tags$div(
      style = "display:flex;justify-content:space-between;align-items:center;",
      shiny::tags$span(style = "font-weight:600;font-size:12px;", lbl),
      shiny::tags$span(style = "font-size:11px;color:#888;",
        paste0("Step ", step, " of ", n_steps))
    ),
    shiny::tags$div(style = "display:flex;gap:4px;margin:8px 0;", prog_dots),
    shiny::tags$div(class = "sb-q-text", style = "margin-bottom:4px;", s$q),
    if (!is.null(s$hint)) shiny::tags$div(class = "sb-hint", s$hint),
    q_widget,
    shiny::tags$div(style = "margin-top:10px;display:flex;gap:6px;",
      if (step > 1)
        shiny::tags$button(class = "sb-btn-sm",
          onclick = "Shiny.setInputValue('sb_step_back', Math.random(), {priority:'event'})",
          "\u2190 Back"),
      if (step < n_steps)
        shiny::tags$button(class = "sb-btn-sm",
          style = "background:#5B9BD5;color:#fff;",
          onclick = "Shiny.setInputValue('sb_step_next', Math.random(), {priority:'event'})",
          "Next \u2192"),
      if (step == n_steps)
        shiny::tags$button(class = "sb-btn-sm",
          style = "background:#70AD47;color:#fff;",
          onclick = "Shiny.setInputValue('sb_step_next', Math.random(), {priority:'event'})",
          "Done \u2713")
    )
  )
}

# yes/no radio with pre-selected value (kept for any remaining callers)
.sb_yn <- function(id, label, selected = "no") {
  shiny::tags$div(
    style = "margin:4px 0;display:flex;align-items:center;gap:8px;",
    shiny::tags$span(style = "font-size:12px;color:#555;flex:1;", label),
    shiny::radioButtons(id, NULL,
      choices  = c("No" = "no", "Yes" = "yes"),
      selected = selected, inline = TRUE)
  )
}

# ---------------------------------------------------------------------------
# Grid mode helper -- what kind of cell selection to capture for this step
# ---------------------------------------------------------------------------

# Derive grid click mode from the step's ui field (not step number).
.sb_grid_mode <- function(type, step, answer = NULL, p = list(),
                          listed_answered = FALSE, listed_answer = NULL,
                          cluster_obs_answer = NULL,
                          n_other_obs = 0L, n_clusters = 1L) {
  steps   <- .sb_steps(type, p, step, listed_answered, listed_answer = listed_answer,
                       cluster_obs_answer = cluster_obs_answer,
                       n_other_obs = n_other_obs, n_clusters = n_clusters)
  n       <- length(steps)
  step    <- max(1L, min(step, n))
  ui      <- steps[[step]]$ui
  switch(ui,
    col_select           = "col",
    row_select           = "row",
    origin_select        = "origin",
    group_origin_select  = "group_origin",
    yn_then_row          = if (identical(answer, "yes")) "row"    else "none",
    yn_then_cols    = if (identical(answer, "yes")) "col"    else "none",
    yn_then_col1       = if (identical(answer, "yes")) "col" else "none",
    yn_then_text_merge = "none",
    yn_then_text_split = "none",
    yn_then_origins = "none",  # origin clicking happens on the next origin_select step
    yn_then_kv      = if (identical(answer, "yes")) "col"    else "none",
    "none")
}

# ---------------------------------------------------------------------------
# Schema code generator -- from rv$vars list
# ---------------------------------------------------------------------------

.sb_gen_code <- function(rv, input = NULL) {
  type <- rv$table_type %||% "tidy"
  v <- rv$vars

  lines <- character(0)

  # setGroups -- one call per row group (fill/fn read from input$* to avoid re-render)
  for (gi in seq_along(rv$groups)) {
    grp <- rv$groups[[gi]]
    if (length(grp$rows) == 0) next
    row_arg <- if (length(grp$rows) == 1) as.character(grp$rows)
               else sprintf("c(%s)", paste(grp$rows, collapse=", "))
    sum_args <- row_arg
    fill <- character(0)
    if (!is.null(input)) {
      if (isTRUE(input[[paste0("grp_fill_down_",  gi)]])) fill <- c(fill, "down")
      if (isTRUE(input[[paste0("grp_fill_up_",    gi)]])) fill <- c(fill, "up")
      if (isTRUE(input[[paste0("grp_fill_right_", gi)]])) fill <- c(fill, "right")
    }
    if (length(fill) > 0)
      sum_args <- paste0(sum_args, sprintf(', fill = c(%s)',
        paste(sprintf('"%s"', fill), collapse=", ")))
    char_fn <- if (!is.null(input)) trimws(input[[paste0("grp_char_fn_", gi)]] %||% "") else ""
    if (nchar(char_fn) > 0)
      sum_args <- paste0(sum_args, sprintf(', character = function(.x) %s', char_fn))
    num_fn <- if (!is.null(input)) trimws(input[[paste0("grp_num_fn_", gi)]] %||% "") else ""
    if (nchar(num_fn) > 0)
      sum_args <- paste0(sum_args, sprintf(', numeric = function(.x) %s', num_fn))
    lines <- c(lines, sprintf('setGroups(rows = .sum(%s))', sum_args))
  }

  if (length(v) == 0) {
    if (length(lines) == 0) return("# Add variables above to generate schema code")
  }

  # setCluster -- emitted when any variable has clustered == "yes" (id var) or cluster_obs == "yes" (obs var)
  clust_vars     <- Filter(function(x) isTRUE(x$props$clustered    == "yes"), v)
  clust_obs_vars <- Filter(function(x) isTRUE(x$props$cluster_obs  == "yes"), v)
  all_clust      <- c(clust_vars, clust_obs_vars)
  if (length(all_clust) > 0) {
    cv   <- all_clust[[1]]
    p_cv <- cv$props
    # for cluster-obs vars, id = "observed" (tabshiftr convention)
    is_clust_obs <- isTRUE(cv$props$cluster_obs == "yes")
    clust_nm <- if (is_clust_obs) "observed"
                else if (nchar(p_cv$name %||% "") > 0) p_cv$name
                else NULL
    origins  <- p_cv$origins %||% list()

    id_arg <- if (!is.null(clust_nm)) sprintf('id = "%s"', clust_nm)
              else 'id = "cluster"'

    # nested cluster args
    grp_nm  <- trimws(p_cv$group_name %||% "")
    member  <- p_cv$member %||% integer(0)
    grp_arg <- if (nchar(grp_nm) > 0) sprintf(', group = "%s"', grp_nm) else ""
    mem_arg <- if (length(member) > 0)
      sprintf(', member = c(%s)', paste(member, collapse=", ")) else ""

    if (length(origins) > 0) {
      lefts <- vapply(origins, `[[`, integer(1), "col")
      tops  <- vapply(origins, `[[`, integer(1), "row")
      top_arg  <- if (length(unique(tops))  == 1) as.character(tops[1])
                  else paste0("c(", paste(tops,  collapse=", "), ")")
      left_arg <- if (length(unique(lefts)) == 1) as.character(lefts[1])
                  else paste0("c(", paste(lefts, collapse=", "), ")")
      lines <- c(lines, sprintf('setCluster(%s%s%s, left = %s, top = %s)',
        id_arg, grp_arg, mem_arg, left_arg, top_arg))
    } else {
      lines <- c(lines, sprintf('setCluster(%s%s%s, left = ..., top = ...)',
        id_arg, grp_arg, mem_arg))
    }

    # emit setIDVar for the group variable (positions from group_origins)
    if (nchar(grp_nm) > 0) {
      gorigs <- p_cv$group_origins %||% list()
      if (length(gorigs) > 0) {
        grow_str <- if (length(gorigs) == 1) as.character(gorigs[[1]]$row)
                    else paste0("c(", paste(vapply(gorigs, `[[`, integer(1), "row"), collapse=", "), ")")
        gcol_str <- if (length(unique(vapply(gorigs, `[[`, integer(1), "col"))) == 1)
          as.character(gorigs[[1]]$col)
          else paste0("c(", paste(vapply(gorigs, `[[`, integer(1), "col"), collapse=", "), ")")
        lines <- c(lines, sprintf('setIDVar(name = "%s", columns = %s, rows = %s)',
          grp_nm, gcol_str, grow_str))
      } else {
        lines <- c(lines, sprintf('setIDVar(name = "%s", columns = ..., rows = ...)', grp_nm))
      }
    }
  }

  is_listed <- isTRUE(grepl("listed", type))

  # TRUE when any obs var answered cluster_obs == "yes" -- all obs vars in this table are cluster IDs
  is_clust_obs_table <- any(vapply(v, function(x)
    x$type == "obs" && isTRUE(x$props$cluster_obs == "yes"), logical(1)))

  # determine the shared key/val cols from whichever obs var answered the listed question
  listed_key_col <- NULL
  listed_val_col <- NULL
  for (.var in v) {
    if (.var$type == "obs" && isTRUE(.var$props$listed == "yes")) {
      listed_key_col <- .var$props$key_col
      listed_val_col <- .var$props$val_col
      break
    }
  }

  for (var in v) {
    p  <- var$props
    nm <- if (nchar(p$name %||% "") > 0) p$name else var$key

    if (var$type == "id") {
      if (isTRUE(p$implicit == "no")) {
        val  <- if (nchar(p$value %||% "") > 0) sprintf('"%s"', p$value) else '"..."'
        args <- sprintf('name = "%s", value = %s', nm, val)
      } else {
        if (isTRUE(p$clustered == "yes")) {
          # columns and rows both come from origins (one entry per block, preserving duplicates)
          origs   <- p$origins %||% list()
          origs_c <- vapply(origs, `[[`, integer(1), "col")
          origs_r <- vapply(origs, `[[`, integer(1), "row")
          col_str <- if (length(origs_c) == 1) as.character(origs_c)
                     else if (length(origs_c) > 1) paste0("c(", paste(origs_c, collapse=", "), ")")
                     else "..."
          row_str <- if (length(origs_r) == 1) as.character(origs_r)
                     else if (length(origs_r) > 1) paste0("c(", paste(origs_r, collapse=", "), ")")
                     else "..."
          args <- sprintf('name = "%s", columns = %s, rows = %s', nm, col_str, row_str)
        } else {
          cols <- p$columns %||% integer(0)
          col_str <- if (length(cols) == 1) as.character(cols)
                     else if (length(cols) > 1) paste0("c(", paste(cols, collapse=", "), ")")
                     else "..."
          args <- sprintf('name = "%s", columns = %s', nm, col_str)
          if (isTRUE(p$wide == "yes") || isTRUE(p$distinct == "yes")) {
            row_str <- if (length(p$rows %||% integer(0)) == 1) as.character(p$rows)
                       else if (length(p$rows) > 1) paste0("c(", paste(p$rows, collapse=", "), ")")
                       else "..."
            args <- paste0(args, sprintf(", rows = %s", row_str))
          }
        }
        if (isTRUE(p$merge_cols == "yes")) {
          mc <- if (nchar(p$merge_char %||% "") > 0) p$merge_char else " "
          args <- paste0(args, sprintf(', merge = "%s"', mc))
        }
        if (isTRUE(p$split == "yes")) {
          sr <- if (nchar(p$split_regex %||% "") > 0) p$split_regex else "..."
          args <- paste0(args, sprintf(', split = "%s"', sr))
        }
        if (isTRUE(p$distinct == "yes"))
          args <- paste0(args, ", distinct = TRUE")
      }
      lines <- c(lines, sprintf('setIDVar(%s)', args))

    } else {
      cols <- p$columns %||% integer(0)
      col_str <- if (length(cols) == 1) as.character(cols)
                 else if (length(cols) > 1) paste0("c(", paste(cols, collapse=", "), ")")
                 else "..."

      if (is_clust_obs_table) {
        # all obs vars are cluster-ID variables: key = "cluster", value = cluster index
        lines <- c(lines,
          sprintf('setObsVar(name = "%s", columns = %s, key = "cluster", value = %s)',
                  nm, col_str,
                  if (!is.null(p$cluster_value)) as.character(as.integer(p$cluster_value)) else "..."))
      } else {
        var_is_listed <- isTRUE(p$listed == "yes") || !is.null(listed_key_col)
        if (var_is_listed) {
          key_col   <- listed_key_col %||% p$key_col %||% "..."
          raw_vcol  <- listed_val_col %||% p$val_col %||% NULL
          val_col   <- if (is.null(raw_vcol)) col_str
                       else if (length(raw_vcol) > 1) paste0("c(", paste(raw_vcol, collapse=", "), ")")
                       else raw_vcol
          key_value <- if (nchar(p$listed_value %||% "") > 0) p$listed_value else "..."
          lines <- c(lines,
            sprintf('setObsVar(name = "%s", columns = %s, key = %s, value = "%s")',
                    nm, val_col, key_col, key_value))
        } else {
          args <- sprintf('name = "%s", columns = %s', nm, col_str)
          fac  <- suppressWarnings(as.numeric(p$factor %||% "1"))
          if (!is.na(fac) && fac != 1)
            args <- paste0(args, sprintf(", factor = %s", fac))
          if (isTRUE(p$top == "yes")) {
            row_str <- if (length(p$rows %||% integer(0)) == 1) as.character(p$rows)
                       else if (length(p$rows) > 1) paste0("c(", paste(p$rows, collapse=", "), ")")
                       else "..."
            args <- paste0(args, sprintf(", top = %s", row_str))
          }
          lines <- c(lines, sprintf('setObsVar(%s)', args))
        }
      }
    }
  }

  # setFormat -- read live input values (not stored in rv to avoid focus-loss re-renders)
  fmt_args <- character(0)
  if (!is.null(input)) {
    dec <- trimws(input$fmt_decimal %||% "")
    if (nchar(dec) > 0)
      fmt_args <- c(fmt_args, sprintf('decimal = "%s"', dec))
    tho <- trimws(input$fmt_thousand %||% "")
    if (nchar(tho) > 0)
      fmt_args <- c(fmt_args, sprintf('thousand = "%s"', tho))
    na_raw <- trimws(input$fmt_na %||% "")
    if (nchar(na_raw) > 0) {
      na_vals <- strsplit(na_raw, "\\s*,\\s*")[[1]]
      fmt_args <- c(fmt_args, sprintf('na_values = c(%s)',
        paste(sprintf('"%s"', na_vals), collapse=", ")))
    }
    zero_raw <- trimws(input$fmt_zero %||% "")
    if (nchar(zero_raw) > 0) {
      zero_vals <- strsplit(zero_raw, "\\s*,\\s*")[[1]]
      fmt_args <- c(fmt_args, sprintf('zero_values = c(%s)',
        paste(sprintf('"%s"', zero_vals), collapse=", ")))
    }
  }
  # Flags -- still in rv (add/delete changes need reactive storage)
  if (nrow(rv$fmt_flags) > 0) {
    # Prefer live input values over rv$fmt_flags (user may have typed without blur)
    fl <- rv$fmt_flags
    if (!is.null(input)) {
      for (i in seq_len(nrow(fl))) {
        live_flag <- input[[paste0("fmt_flag_",    i)]] %||% fl$flag[i]
        live_val  <- input[[paste0("fmt_flagval_", i)]] %||% fl$value[i]
        fl$flag[i]  <- live_flag
        fl$value[i] <- live_val
      }
    }
    fl <- fl[nchar(fl$flag) > 0, , drop=FALSE]
    if (nrow(fl) > 0) {
      fmt_args <- c(fmt_args, sprintf(
        'flags = data.frame(flag=c(%s), value=c(%s))',
        paste(sprintf('"%s"', fl$flag), collapse=", "),
        paste(sprintf('"%s"', fl$value), collapse=", ")))
    }
  }
  if (length(fmt_args) > 0)
    lines <- c(lines, sprintf("setFormat(%s)", paste(fmt_args, collapse=", ")))

  # setFilter -- one call per filter
  for (fi in seq_along(rv$filters)) {
    flt      <- rv$filters[[fi]]
    flt_args <- character(0)
    # invert read from input (not stored in rv -- avoids re-render on checkbox click)
    inv <- if (!is.null(input)) isTRUE(input[[paste0("flt_invert_", fi)]]) else isTRUE(flt$invert)
    # operator -- only for 2nd+ filters
    if (fi > 1 && !is.null(flt$operator)) {
      op_str <- if (flt$operator == "|") "operator = `|`" else "operator = `&`"
      flt_args <- c(flt_args, op_str)
    }
    if (isTRUE(flt$use_find)) {
      # .find() based -- search_in determines the col/row arg to .find();
      # find_select determines whether the result is rows = or columns =
      pat <- flt$find_pattern
      if (nchar(pat) > 0) {
        search_in <- flt$find_search_in %||% "row"
        sel       <- flt$find_select    %||% "rows"
        dim_num   <- suppressWarnings(as.integer(flt$find_dim))
        find_args <- sprintf('pattern = "%s"', pat)
        if (!is.na(dim_num)) {
          if (search_in == "col")
            find_args <- paste0(find_args, sprintf(", col = %d", dim_num))
          else
            find_args <- paste0(find_args, sprintf(", row = %d", dim_num))
        }
        if (inv)
          find_args <- paste0(find_args, ", invert = TRUE")
        flt_args <- c(flt_args, sprintf("%s = .find(%s)", sel, find_args))
      }
    } else {
      # explicit positions -- UI says "click to exclude", so rows/cols are the excluded set.
      # setFilter keeps the specified rows by default, so we need invert = TRUE to exclude.
      # The "Invert" checkbox flips that: checked = keep only these (no invert needed).
      emit_invert <- !inv   # click-mode default is exclude -> invert; checkbox flips it
      if (length(flt$rows) > 0) {
        row_str <- if (length(flt$rows) == 1) as.character(flt$rows)
                   else paste0("c(", paste(flt$rows, collapse=", "), ")")
        flt_args <- c(flt_args, sprintf("rows = %s", row_str))
      }
      if (length(flt$cols) > 0) {
        col_str <- if (length(flt$cols) == 1) as.character(flt$cols)
                   else paste0("c(", paste(flt$cols, collapse=", "), ")")
        flt_args <- c(flt_args, sprintf("columns = %s", col_str))
      }
      if (emit_invert)
        flt_args <- c(flt_args, "invert = TRUE")
    }
    if (length(flt_args) > 0)
      lines <- c(lines, sprintf("setFilter(%s)", paste(flt_args, collapse=", ")))
  }

  if (length(lines) == 0) return("# Add variables to generate schema code")
  if (length(lines) == 1) return(paste0("schema <- ", lines[1]))

  first <- paste0("schema <- ", lines[1], " |>")
  rest  <- vapply(seq(2, length(lines)), function(i) {
    if (i < length(lines)) paste0("  ", lines[i], " |>")
    else paste0("  ", lines[i])
  }, character(1))
  paste(c(first, rest), collapse = "\n")
}

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

.sb_swatch <- function(color, label) {
  shiny::tags$div(class = "sb-swatch",
    shiny::tags$div(class = "sb-swatch-box",
                    style = paste0("background:", color, ";")),
    label)
}

`%||%` <- function(a, b) if (!is.null(a)) a else b
