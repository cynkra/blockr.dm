# --- Missing / empty handling ----------------------------------------------
#
# NA (and the empty string "") are surfaced as selectable options in the value
# picker, the same way Excel shows a "(Blanks)" entry. This lets a user pick,
# for a flag like TRTEMFL: "Y" only, missing only, or both (multi-select).
#
# DESIGN NOTE — why tokens, not a data sentinel.
# The values are carried as the literal UI tokens "<NA>" / "<empty>" (mirroring
# blockr.dplyr's filter block) and translated, when building the filter
# expression, into real `is.na(col)` / `col == ""` tests. The data is never
# touched. We deliberately do NOT follow the crossfilter approach of rewriting
# missings to a sentinel string in the column (`col[is.na(col)] <- "__NA__"`):
#   * that collides with genuine text "NA" (North America, Not Applicable, Na),
#   * it forces the column to character and loses the original type, and
#   * it produces dishonest filter code (`col %in% "NA"` instead of is.na()).
# The crossfilter sentinel exists only because that block is chart-driven and
# needs NA to be a concrete, clickable category that round-trips through JS;
# a plain value filter has no such constraint, so `is.na()` is the right tool.
# Tokens are appended AFTER the real values so single-select still defaults to
# a real value (e.g. "Y", not missing); "" is represented solely via the
# "<empty>" token, not as a blank-looking real option.
VALUE_FILTER_NA <- "<NA>"        # nolint: object_name_linter.
VALUE_FILTER_EMPTY <- "<empty>"  # nolint: object_name_linter.

#' Value filter block
#'
#' Minimal value filter for data frames or `dm` objects. The columns to filter
#' on are chosen behind the gear icon (top-right of the block). Each column
#' can be toggled between single-select (always constrains — auto-picks first
#' value) and multi-select (empty selection passes through).
#'
#' When the upstream input is a `dm`, the gear popover gains a table selector
#' so columns can be picked from any of the dm's tables. The emitted filter
#' goes through `dm::dm_filter()`, so the restriction cascades through foreign
#' keys to related tables.
#'
#' Gear/popover UX, select widget, and click-through pill styling are reused
#' from `blockr.dplyr` to match the look-and-feel of the crossfilter and dplyr
#' transform blocks.
#'
#' This is the value-selection member of the relational filter family,
#' alongside [new_dm_filter_block()] (expression-style) and
#' [new_crossfilter_block()]. It lives in `blockr.dm` because its `dm`
#' path depends on `dm`; it is fully usable in a dashboard by composing it
#' onto a board.
#'
#' @param state List with `columns` — a list of column-object entries — and
#'   `operator` ("&" or "|", default "&") controlling how the per-column
#'   conditions combine on a data-frame input (same AND/OR pill as the
#'   blockr.dplyr filter block; a builder option, so it sits in the gear
#'   popover and shows once two or more fields are active). On a `dm` input
#'   the operator is ignored: the restriction cascades per-table through
#'   `dm::dm_filter()`, whose chaining is inherently AND, so the pill is not
#'   offered there. Each column entry has `name` (column name), `table`
#'   (source table; only when input is a `dm`), `mode` (`"single"` or
#'   `"multi"`), and `values` (character vector of selected values). Logical
#'   columns in single mode render as compact include-style checkbox rows:
#'   checked requires `col %in% TRUE`, unchecked means no constraint from
#'   that column (NOT a `FALSE` filter). To target FALSE/NA explicitly,
#'   flip the column to Multi (values plus the `<NA>` token) or use the
#'   blockr.dplyr filter block. (Legacy saved states with a logical
#'   single-select value of "FALSE" still evaluate as `!(col %in% TRUE)`.)
#'   Old-style state (`list(columns=character, modes=list, values=list)`)
#'   is auto-migrated via [migrate_value_filter_state()] for backward
#'   compatibility.
#' @param ... Additional arguments forwarded to [blockr.core::new_transform_block()].
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(
#'     new_value_filter_block(
#'       state = list(
#'         columns = list(
#'           list(name = "Species", mode = "single", values = "setosa")
#'         )
#'       )
#'     ),
#'     data = list(data = iris)
#'   )
#' }
#'
#' @importFrom blockr.dplyr blockr_core_js_dep blockr_blocks_css_dep blockr_select_dep
#' @importFrom shiny moduleServer reactive reactiveVal observeEvent NS div
#'   tagList
#' @importFrom htmltools htmlDependency
#'
#' @export
new_value_filter_block <- function(
  state = list(columns = list()),
  ...
) {
  blockr.core::new_transform_block(
    value_filter_server(migrate_value_filter_state(state), drill = FALSE),
    value_filter_ui(drill = FALSE),
    dat_valid = value_filter_dat_valid,
    class = "value_filter_block",
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

# --- shared implementation ---------------------------------------------------
#
# `new_value_filter_block()` and `new_drill_filter_block()` are ONE block with
# two receiving ends, so they share a server and a UI rather than a copy each.
# The difference is `drill`: the drill target resolves an incoming claim's table
# itself and reports a claim it cannot place. Everything else -- the state
# shape, the widget, the lazy value loading, the expression discipline -- is the
# same code.
#
# Two constructors and not one argument, because the whole point of the drill
# target is that a board has exactly ONE and it can be found without being
# named. `ctrl_targets()` looks blocks up BY CLASS, so the distinction has to be
# in the class vector; a flag on the value filter would leave the sender
# counting candidates again, which is the bug this exists to remove.
#
# They must also stay separate TOP-LEVEL constructors: `new_transform_block()`
# records the ctor from `sys.parent()`, so a wrapper delegating to the other
# constructor would serialize as that one, and a saved board would restore a
# drill filter as a plain value filter.

#' @noRd
value_filter_dat_valid <- function(data) {
  if (!is.data.frame(data) && !inherits(data, "dm")) {
    stop("Input must be a data frame or a dm")
  }
}

#' The block server, shared by the value filter and the drill filter.
#'
#' @param state The migrated filter state.
#' @param drill Resolve an incoming claim's dm table, and report one that
#'   cannot be placed. `TRUE` only for [new_drill_filter_block()].
#' @noRd
value_filter_server <- function(state, drill = FALSE) {
  function(id, data) {
    shiny::moduleServer(id, function(input, output, session) {
      ns <- session$ns
      r_state <- shiny::reactiveVal(state)

      self_write <- new.env(parent = emptyenv())
      self_write$active <- FALSE

      # DRILL TARGET ONLY. A claim arriving over the control channel names a
      # COLUMN and, on a dm, no table: the sender does not hold the data model,
      # so it cannot know which table the column lives in. `make_dm_filter_expr()`
      # SKIPS any entry whose table is empty, which means an unresolved claim is
      # dropped without a word. Resolve it here, where the dm is, and say so
      # when it cannot be resolved -- the derived-column case, e.g. a composer
      # table claiming `DEATH` against a dm that only carries `DTHFL`.
      r_drill_note <- shiny::reactiveVal(character())

      # Send column metadata (names/labels/table) on data change. The
      # per-column unique-value lists are NOT shipped here — they load
      # lazily on first dropdown-open (see the request handler below). In
      # dm mode this is what avoids paying N-tables x uniques at startup,
      # and never collects a remote (e.g. DuckDB-backed) table.
      shiny::observeEvent(data(), {
        d <- data()
        meta <- build_column_meta(d)
        if (is.null(meta)) return()
        session$sendCustomMessage(
          "bi-filter-columns",
          list(
            id      = ns("filter_input"),
            columns = meta$columns,
            is_dm   = meta$is_dm
          )
        )
        # Re-apply single-select rule against fresh data.
        s <- enforce_single_rule(r_state(), d)
        if (!identical(s, r_state())) {
          self_write$active <- FALSE
          r_state(s)
        }
      })

      # The other half of the same observer discipline: `enforce_single_rule()`
      # above derives the COLUMN SELECTION from `data()` and parks it in
      # `r_state`; this derives the expression SHAPE (dm vs data frame, plus
      # the per-column types that drive the value cast) and parks it here.
      # Between them, `expr` below reads only reactiveVals and never `data()`.
      #
      # Why it matters: blockr.core skips re-evaluating a block only when the
      # expression it is handed is the SAME OBJECT as last time (`same_ref()`,
      # see blockr.core/R/block-server.R). `make_filter_block_expr()` builds
      # with `bquote()`, which allocates a fresh call tree on every read, so
      # an `expr` that depended on `data()` re-ran -- and re-evaluated this
      # block plus everything downstream -- on every spurious upstream
      # invalidation (blockr.dock's view switches churn the chain with
      # byte-for-byte identical data). Keyed on a reactiveVal that only
      # changes when the shape actually changes, `expr` is not invalidated by
      # that churn and hands back its cached object. See
      # blockr.cdex/dev/profiling-plan.md, "Settled" item 8.
      #
      # A deliberately SEPARATE reactiveVal from `r_state`: `r_state` is the
      # JS-synced blob that round-trips to the client (sendCustomMessage /
      # `self_write`), and this is a purely server-side derived decision.
      #
      # The failure branch MUST write an explicit "not ready" marker rather
      # than skip the write: silently leaving the last good shape in place
      # would make `expr` serve a STALE expression exactly where it has to
      # propagate the upstream's silent stop, which blockr.core reads as
      # "this block is waiting" (not "this block has output").
      shape_rv <- shiny::reactiveVal(NULL)

      shiny::observe({
        shape_rv(
          tryCatch(
            # NB: a NULL / not-yet-ready upstream is a VALID shape, not a
            # failure -- it builds the data-frame branch, which is the
            # documented restore behaviour (see the note on `expr`).
            list(ok = TRUE, shape = filter_input_shape(data())),
            # An upstream `req()` / error must keep reaching blockr.core; it
            # is captured here (an erroring observer would kill the session)
            # and re-raised from `expr` below, unchanged.
            error = function(e) list(ok = FALSE, cond = e)
          )
        )
      })

      # JS -> R: lazy value request for one column (fires on dropdown-open).
      # `key` is the qualified key ("table.column" for a dm, bare column
      # name otherwise); only that single column's values are computed.
      shiny::observeEvent(input$filter_input_request_values, {
        key <- input$filter_input_request_values
        opts <- column_values(shiny::isolate(data()), key)
        if (is.null(opts)) return()
        session$sendCustomMessage(
          "bi-filter-column-values",
          list(id = ns("filter_input"), key = key, values = opts)
        )
      })

      # JS -> R: the widget binding announced itself (fires from the
      # binding's initialize(), every time the element is (re)bound). In a
      # deferred dock panel the block's script arrives with the panel on
      # FIRST VISIT — any push flushed before that had no registered
      # handler and was dropped by Shiny outright, out of reach of the JS
      # replay queue. Re-send column metadata and the current state on
      # announce; the widget applies latest-wins, so the boot-path
      # duplicate is harmless.
      shiny::observeEvent(input$filter_input_ready, {
        d <- tryCatch(data(), error = function(e) NULL)
        meta <- build_column_meta(d)
        if (!is.null(meta)) {
          session$sendCustomMessage(
            "bi-filter-columns",
            list(
              id      = ns("filter_input"),
              columns = meta$columns,
                is_dm   = meta$is_dm
            )
          )
        }
        session$sendCustomMessage(
          "bi-filter-update",
          list(id    = ns("filter_input"),
               state = normalize_state_for_json(r_state()))
        )
      })

      # JS -> R: user changed state.
      shiny::observeEvent(input$filter_input, {
        incoming <- migrate_value_filter_state(input$filter_input)
        s <- enforce_single_rule(incoming, shiny::isolate(data()))
        # Suppress the R->JS echo only when the server left the JS-sent state
        # untouched. When enforce_single_rule fills a single-select default
        # (the lazy widget can no longer prefill it client-side) or drops a
        # stale entry, JS still holds the un-corrected state — so we MUST
        # echo the correction back, or the widget shows an empty selection
        # while the filter is silently applied.
        self_write$active <- identical(s$columns, incoming$columns)
        r_state(s)
      })

      # R -> JS: external control or server-side rewrite.
      shiny::observeEvent(r_state(), {
        if (self_write$active) {
          self_write$active <- FALSE
        } else {
          session$sendCustomMessage(
            "bi-filter-update",
            list(id    = ns("filter_input"),
                 state = normalize_state_for_json(r_state()))
          )
        }
      })

      # Resolution reads `shape_rv()` -- the cached 0-row templates plus the
      # dm's primary keys -- and never `data()`, for the reason `expr` does not
      # read it either: a claim is then re-resolved when the SHAPE changes, not
      # on every upstream churn. Writing `r_state` from an observer that also
      # reads it converges because resolution is idempotent: the second pass is
      # `identical()` and stops there.
      if (isTRUE(drill)) {

        shiny::observe({

          st <- r_state()
          derived <- shape_rv()

          if (is.null(derived) || !isTRUE(derived$ok)) {
            return()
          }

          res <- resolve_claim_tables(st, derived$shape)
          r_drill_note(res$unresolved)

          if (!identical(res$state, st)) {
            self_write$active <- FALSE
            r_state(res$state)
          }
        })

        output$drill_note <- shiny::renderUI(drill_note_ui(r_drill_note()))
      }

      list(
        # NB: the expression *shape* depends on the input type — a data frame
        # yields `dplyr::filter()`, a `dm` yields `dm::dm_filter()`, and value
        # casting reads the column's type. So `expr` must track the input, NOT
        # isolate it: on restore the upstream is often not ready yet (NULL), in
        # which case the df branch is built; if the real input turns out to be
        # a `dm`, evaluating that wrong-shaped expression errors until the
        # value is re-edited. It tracks it through `shape_rv()` (see the
        # observer above), never by reading `data()` here — that is what keeps
        # the expression object stable across spurious upstream
        # invalidations, so blockr.core's `same_ref()` skip check succeeds.
        expr = shiny::reactive({
          derived <- shape_rv()
          shiny::req(derived)
          # The upstream failed: re-raise its condition verbatim (a `req()`
          # silent stop stays a silent stop, i.e. "waiting").
          if (!isTRUE(derived$ok)) stop(derived$cond)
          make_filter_expr_from_shape(
            r_state()$columns %||% list(),
            derived$shape,
            operator = r_state()$operator %||% "&"
          )
        }),
        state = list(state = r_state)
      )
    })
  }
}

#' The block UI, shared by the value filter and the drill filter.
#' @noRd
value_filter_ui <- function(drill = FALSE) {
  function(id) {
    shiny::tagList(
      blockr.dplyr::blockr_core_js_dep(),
      blockr.dplyr::blockr_blocks_css_dep(),
      blockr.dplyr::blockr_select_dep(),
      value_filter_block_dep(),
      shiny::div(
        class = "block-container",
        shiny::div(
          id = shiny::NS(id, "filter_input"),
          class = "bi-filter-container"
        ),
        if (isTRUE(drill)) shiny::uiOutput(shiny::NS(id, "drill_note"))
      )
    )
  }
}


#' Migrate a pre-column-object filter state into the new column-object shape.
#'
#' Old shape:
#'   `list(columns = c("A", "B"), modes = list(A = "single", B = "multi"),
#'         values = list(A = "x", B = c("y","z")))`
#' New shape:
#'   `list(columns = list(list(name="A", mode="single", values="x"),
#'                        list(name="B", mode="multi",  values=c("y","z"))))`
#'
#' Idempotent: passing a new-shape state returns it unchanged (with the
#' `operator` field normalized to "&" or "|", defaulting to "&").
#'
#' @param state Filter state (old or new shape).
#' @return Filter state in the new column-object shape.
#'
#' @examples
#' migrate_value_filter_state(
#'   list(columns = "Species",
#'        modes   = list(Species = "single"),
#'        values  = list(Species = "setosa"))
#' )
#'
#' @export
migrate_value_filter_state <- function(state) {
  if (is.null(state)) {
    return(list(columns = list(), operator = "&"))
  }
  op <- state$operator %||% "&"
  if (!identical(op, "|")) op <- "&"
  cols <- state$columns
  # Already new shape: `columns` is a list of column-object entries.
  if (is.list(cols) && length(cols) > 0L &&
      is.list(cols[[1L]]) && !is.null(cols[[1L]]$name)) {
    return(list(columns = cols, operator = op))
  }
  if (is.list(cols) && length(cols) == 0L && is.null(state$modes) &&
      is.null(state$values)) {
    return(list(columns = list(), operator = op))
  }
  cols_vec <- as.character(cols %||% character())
  modes <- state$modes %||% list()
  values <- state$values %||% list()
  entries <- lapply(cols_vec, function(cn) {
    list(
      name   = cn,
      mode   = modes[[cn]] %||% "single",
      values = as.character(values[[cn]] %||% character())
    )
  })
  list(columns = entries, operator = op)
}

#' Build cheap column metadata (names/labels/table) for the JS side.
#'
#' Returns a list with `columns` (metadata entries) plus `is_dm` so the JS
#' side can render the dm variant. Per-column unique values are deliberately
#' NOT computed here — they load lazily on dropdown-open via [column_values()].
#' For a `dm` input, column metadata entries include `table` and `column`
#' fields and the `value` key is `"table.column"`.
#'
#' @noRd
build_column_meta <- function(data) {
  if (inherits(data, "dm")) {
    build_column_meta_dm(data)
  } else if (is.data.frame(data) && ncol(data) > 0L) {
    build_column_meta_df(data)
  } else {
    NULL
  }
}

#' @noRd
build_column_meta_df <- function(df) {
  cols <- lapply(names(df), function(cn) {
    list(
      value = cn,
      label = column_label(df[[cn]]),
      type  = column_meta_type(df[[cn]])
    )
  })
  list(columns = cols, is_dm = FALSE)
}

#' @noRd
build_column_meta_dm <- function(dm_obj) {
  if (!requireNamespace("dm", quietly = TRUE)) {
    stop("Package 'dm' is required for dm input. ",
         "Install with install.packages('dm').")
  }
  tbls <- dm::dm_get_tables(dm_obj)
  cols <- list()
  for (tbl_nm in names(tbls)) {
    # A 0-row template carries column names and storage types via one cheap
    # LIMIT 0 query, so a remote (lazy) table is never collected just to
    # enumerate its columns.
    src <- tbls[[tbl_nm]]
    head0 <- table_head0(src)
    if (ncol(head0) == 0L) next
    for (cn in names(head0)) {
      # The LABEL must come off the ORIGINAL column, not the template:
      # `[.data.frame` row-subsetting drops a column's attributes, so reading
      # it off the 0-row template silently yields "" and the field renders
      # with no label at all. Taking `src[[cn]]` is free -- it references the
      # column, it does not copy it. A lazy table has no R attributes to lose,
      # so the template stands in there.
      lbl_src <- if (is.data.frame(src) && cn %in% names(src)) src[[cn]] else head0[[cn]]
      cols[[length(cols) + 1L]] <- list(
        value  = paste0(tbl_nm, ".", cn),
        label  = column_label(lbl_src),
        type   = column_meta_type(head0[[cn]]),
        table  = tbl_nm,
        column = cn
      )
    }
  }
  list(columns = cols, is_dm = TRUE)
}

#' A 0-row, in-memory template of a (possibly lazy) table.
#'
#' For a remote `tbl_lazy` this issues a single `LIMIT 0` query and collects
#' the result, so column names/types/labels are obtained without pulling row
#' data. In-memory tables are sliced to 0 rows directly.
#' @noRd
table_head0 <- function(tbl) {
  if (inherits(tbl, "tbl_lazy")) {
    return(as.data.frame(dplyr::collect(utils::head(tbl, 0L))))
  }
  as.data.frame(tbl)[0L, , drop = FALSE]
}


#' One column's label attribute as a length-1 character ("" if none).
#' @noRd
column_label <- function(col) {
  lbl <- attr(col, "label", exact = TRUE)
  if (is.null(lbl)) "" else as.character(lbl)[1L]
}

#' Coarse column type for the JS side: "logical" columns render as a
#' click-to-cycle value pill instead of a dropdown; everything else is "".
#' @noRd
column_meta_type <- function(col) {
  if (is.logical(col)) "logical" else ""
}

#' Lazily compute the value options for one column, by qualified key.
#'
#' `key` is the qualified key the JS side sends on dropdown-open: `"table.col"`
#' for a `dm`, the bare column name for a data frame. Returns the option list
#' for that single column (see [unique_value_options()]), or `NULL` if the key
#' no longer resolves. For a remote (lazy) dm table only that one column's
#' distinct values are pulled (push-down), never the whole table.
#' @noRd
column_values <- function(data, key) {
  if (is.null(key) || !nzchar(key)) return(NULL)
  if (inherits(data, "dm")) {
    if (!requireNamespace("dm", quietly = TRUE)) return(NULL)
    parts <- strsplit(key, ".", fixed = TRUE)[[1L]]
    if (length(parts) < 2L) return(NULL)
    tbl_nm <- parts[[1L]]
    cn <- paste(parts[-1L], collapse = ".")
    tbls <- dm::dm_get_tables(data)
    if (!tbl_nm %in% names(tbls)) return(NULL)
    src <- lazy_column_vector(tbls[[tbl_nm]], cn)
  } else if (is.data.frame(data)) {
    if (!key %in% names(data)) return(NULL)
    src <- data[[key]]
  } else {
    return(NULL)
  }
  if (is.null(src)) return(NULL)
  unique_value_options(src)
}

#' Pull one column as an in-memory vector from a (possibly lazy) table.
#'
#' For a `tbl_lazy` the distinct values of just that column are collected
#' (the DISTINCT is pushed to the backend); in-memory tables index directly.
#' Returns `NULL` if the column is absent.
#' @noRd
lazy_column_vector <- function(tbl, cn) {
  if (inherits(tbl, "tbl_lazy")) {
    if (!cn %in% colnames(tbl)) return(NULL)
    out <- dplyr::collect(
      dplyr::distinct(dplyr::select(tbl, dplyr::all_of(cn)))
    )
    return(out[[cn]])
  }
  df <- as.data.frame(tbl)
  if (!cn %in% names(df)) return(NULL)
  df[[cn]]
}

#' Distinct, stably-ordered value options for one column.
#'
#' Honors haven-style `labels` attributes by emitting `{value, label}` pairs.
#' Otherwise returns a plain list of stringified unique values. When the column
#' contains missings (or, for text columns, empty strings) the corresponding
#' `<NA>` / `<empty>` token is appended after the real values so they can be
#' selected; see the design note at the top of this file.
#' @noRd
unique_value_options <- function(col) {
  labs <- attr(col, "labels", exact = TRUE)
  is_text <- is.character(col) || is.factor(col)
  has_na <- anyNA(col)
  uv <- unique(col)
  uv <- uv[!is.na(uv)]
  # "" is represented via the <empty> token, not as a blank-looking option.
  has_empty <- is_text && any(as.character(uv) == "")
  if (is_text) uv <- uv[as.character(uv) != ""]
  if (length(uv) > 0L) {
    if (is.factor(col)) {
      uv <- uv[order(match(as.character(uv), levels(col)))]
    } else if (is.numeric(uv) || is.logical(uv)) {
      uv <- sort(uv)
    } else {
      uv <- sort(as.character(uv))
    }
  }
  opts <- if (!is.null(labs) && is.vector(labs) && !is.null(names(labs))) {
    lab_names <- names(labs)
    lapply(uv, function(v) {
      idx <- match(v, labs)
      list(
        value = as.character(v),
        label = if (is.na(idx)) "" else as.character(lab_names[idx])
      )
    })
  } else {
    as.list(as.character(uv))
  }
  if (has_empty) opts <- c(opts, list(VALUE_FILTER_EMPTY))
  if (has_na) opts <- c(opts, list(VALUE_FILTER_NA))
  opts
}

#' Look up the source vector for a column-object entry.
#'
#' Returns `NULL` if the entry's table or column no longer exists in the
#' upstream data (data frame or dm).
#' @noRd
column_source <- function(data, entry) {
  if (inherits(data, "dm")) {
    if (!requireNamespace("dm", quietly = TRUE)) return(NULL)
    tbl_nm <- entry$table %||% ""
    if (!nzchar(tbl_nm)) return(NULL)
    tbls <- dm::dm_get_tables(data)
    if (!tbl_nm %in% names(tbls)) return(NULL)
    # Lazy-safe: pulls only this column's distinct values for a remote table,
    # never the whole table (single-select default-fill touches active
    # entries only, but on a remote dm even one full collect is wasteful).
    lazy_column_vector(tbls[[tbl_nm]], entry$name)
  } else if (is.data.frame(data)) {
    if (!entry$name %in% names(data)) return(NULL)
    data[[entry$name]]
  } else {
    NULL
  }
}

#' First (sorted, non-NA) value of a source vector, as character.
#' @noRd
first_value <- function(src) {
  if (is.null(src)) return(NULL)
  uv <- unique(src)
  uv <- uv[!is.na(uv)]
  if (length(uv) == 0L) return(NULL)
  if (is.factor(src)) {
    uv <- uv[order(match(as.character(uv), levels(src)))]
  } else if (is.numeric(uv) || is.logical(uv)) {
    uv <- sort(uv)
  } else {
    uv <- sort(as.character(uv))
  }
  as.character(uv[[1L]])
}

#' Enforce "single-select always has a value" and drop schema-missing entries.
#' Preserves the state's `operator` field. An unchecked logical
#' include-checkbox is never value-filled — empty is the deliberate
#' pass-through state.
#' @noRd
enforce_single_rule <- function(state, data) {
  if (is.null(state)) return(list(columns = list(), operator = "&"))
  op <- state$operator %||% "&"
  if (!identical(op, "|")) op <- "&"
  cols <- state$columns %||% list()
  if (length(cols) == 0L) return(list(columns = cols, operator = op))
  # Drop entries naming a column that is not in the upstream data.
  cols <- Filter(function(entry) !is.null(column_source(data, entry)), cols)
  for (i in seq_along(cols)) {
    entry <- cols[[i]]
    mode <- entry$mode %||% "single"
    if (!identical(mode, "single")) next
    v <- entry$values
    if (length(v) > 0L && !is.null(v)) next
    src <- column_source(data, entry)
    # Logical columns are include-style checkboxes: empty values = unchecked
    # = "no constraint from this flag" — a deliberate state, never filled.
    # (The JS side adds fresh flag entries as checked/TRUE.)
    if (is.logical(src)) next
    fv <- first_value(src)
    if (!is.null(fv)) cols[[i]]$values <- fv
  }
  list(columns = cols, operator = op)
}

#' Everything the expression builder needs to know about the input.
#'
#' The builder reads exactly two things off the live data: the BRANCH (a `dm`
#' yields `dm::dm_filter()` calls, anything else `dplyr::filter()`) and, per
#' table, the column names and types that drive the per-column value cast in
#' `column_condition_expr()`. Both are captured here as a plain list of 0-row
#' column templates, so the block server can derive the decision once per data
#' change (in an observer) instead of re-reading `data()` from `expr` — see the
#' note on the observer in [new_value_filter_block()].
#'
#' Two equal-but-freshly-built inputs produce an `identical()` shape, which is
#' what lets the `reactiveVal` holding it treat a spurious re-derivation as a
#' no-op. A 0-row template also keeps a remote (lazy) dm table from being
#' collected just to read its column types, the same trick `table_head0()` is
#' already used for in `build_column_meta_dm()`.
#'
#' @noRd
filter_input_shape <- function(data) {
  if (inherits(data, "dm")) {
    if (!requireNamespace("dm", quietly = TRUE)) {
      stop("Package 'dm' is required for dm input. ",
           "Install with install.packages('dm').")
    }
    return(list(
      is_dm  = TRUE,
      tables = lapply(dm::dm_get_tables(data), table_head0),
      # For the drill target only, and cheap: the key table is metadata the dm
      # already holds, no query. It decides which table a claim goes to when
      # several carry the column (see `claim_table_for()`).
      pks    = dm_pk_map(data)
    ))
  }
  list(
    is_dm = FALSE,
    df    = if (is.data.frame(data)) data[0L, , drop = FALSE] else NULL
  )
}

#' Build the filter expression — branches on input type.
#'
#' Data frame: `dplyr::filter(data, <combined-cond>)`, with the per-column
#' conditions joined by `operator` ("&" or "|"). Empty state =
#' `dplyr::filter(data, TRUE)`.
#'
#' dm: chained `dm::dm_filter()` calls, one per table, with same-table
#' conditions joined by `&`. The `operator` is deliberately ignored here —
#' `dm_filter()` chaining cascades per table through foreign keys, which is
#' inherently AND; a cross-table OR is not expressible in that form (the UI
#' hides the AND/OR pill for dm input accordingly). Empty state =
#' `dm::dm_filter(data)` (identity).
#' @noRd
make_filter_block_expr <- function(columns, data, operator = "&") {
  make_filter_expr_from_shape(
    columns, filter_input_shape(data), operator = operator
  )
}

# The data SLOT, as blockr.core's `.()` placeholder.
#
# These blocks are expr_type = "bquoted", where blockr.core substitutes
# `.()` placeholders and NOTHING else. A bare `data` symbol therefore
# resolves at RUNTIME (the block's eval env binds it) and looks perfectly
# fine in the app, while the EXPORTED code keeps the bare name: in a
# generated script or a report chunk it falls through to utils::data, the
# chunk "succeeds" holding a function, and every dependent dies with "no
# applicable method for 'filter' applied to an object of class \"function\"".
# Built by hand rather than through bbquote, which walks the expression and
# cannot be given one that defines a function.
data_slot <- function() {
  call(".", as.name("data"))
}

#' `make_filter_block_expr()` against a pre-derived `filter_input_shape()`.
#'
#' This is the form the block server uses: the shape is computed once per data
#' change in an observer, so building the expression touches no live data.
#' @noRd
make_filter_expr_from_shape <- function(columns, shape, operator = "&") {
  if (is.null(shape)) shape <- list(is_dm = FALSE, df = NULL)
  if (length(columns) == 0L) {
    if (isTRUE(shape$is_dm)) {
      return(bquote(dm::dm_filter(.(d)), list(d = data_slot())))
    }
    return(bquote(dplyr::filter(.(d), TRUE), list(d = data_slot())))
  }
  if (isTRUE(shape$is_dm)) {
    make_dm_filter_expr(columns, shape$tables)
  } else {
    make_df_filter_expr(columns, shape$df, operator = operator)
  }
}

#' @noRd
make_df_filter_expr <- function(columns, df, operator = "&") {
  exprs <- list()
  for (entry in columns) {
    cond <- column_condition_expr(entry, df)
    if (!is.null(cond)) exprs[[length(exprs) + 1L]] <- cond
  }
  if (length(exprs) == 0L) {
    return(bquote(dplyr::filter(.(d), TRUE), list(d = data_slot())))
  }
  combined <- combine_conds(exprs, operator)
  as.call(list(quote(dplyr::filter), data_slot(), combined))
}

#' @param tbls_dm Named list of 0-row column templates, one per dm table (the
#'   `tables` element of a `filter_input_shape()`).
#' @noRd
make_dm_filter_expr <- function(columns, tbls_dm) {
  by_table <- list()
  for (entry in columns) {
    tbl <- entry$table %||% ""
    if (!nzchar(tbl) || !tbl %in% names(tbls_dm)) next
    src_tbl <- tbls_dm[[tbl]]
    cond <- column_condition_expr(entry, src_tbl)
    if (is.null(cond)) next
    by_table[[tbl]] <- c(by_table[[tbl]], list(cond))
  }
  if (length(by_table) == 0L) {
    return(bquote(dm::dm_filter(.(d)), list(d = data_slot())))
  }
  # Build nested dm::dm_filter() calls. Table name becomes a named argument
  # — this is how dm::dm_filter() targets a table via tidy-eval.
  result <- data_slot()
  for (tbl in names(by_table)) {
    cond <- combine_conds(by_table[[tbl]], "&")
    cl <- call("dm_filter", result)
    cl[[tbl]] <- cond
    cl[[1L]] <- quote(dm::dm_filter)
    result <- cl
  }
  result
}

#' Build the condition for a single column-object entry.
#'
#' Real values become `<col> %in% <values>` (coerced to the source column's
#' type when safe — numeric, logical, integer). The `<NA>` / `<empty>` tokens
#' become `is.na(col)` / `col == ""` and are OR'd in (see the design note at
#' the top of this file). Returns the bare `%in%` call when no tokens are
#' selected, so the common case is unchanged.
#'
#' Special case — single-mode LOGICAL columns (the switch UI): ON emits
#' `col %in% TRUE` (missings excluded, as always), OFF emits
#' `!(col %in% TRUE)` so missings count as FALSE. A flag that was never set
#' is not "treatment-emergent", so the OFF side must keep NA rows; users who
#' need NA as its own category flip the column to Multi (the `<NA>` token)
#' or use the dplyr filter block.
#' @noRd
column_condition_expr <- function(entry, src_df) {
  v <- entry$values
  if (is.null(v) || length(v) == 0L) return(NULL)
  v <- as.character(v)
  col <- entry$name
  if (is.null(col) || !nzchar(col)) return(NULL)

  has_na <- VALUE_FILTER_NA %in% v
  has_empty <- VALUE_FILTER_EMPTY %in% v
  real <- v[!v %in% c(VALUE_FILTER_NA, VALUE_FILTER_EMPTY)]

  sym <- as.name(col)

  mode <- entry$mode %||% "single"

  if (identical(mode, "single") && length(real) == 1L &&
      !has_na && !has_empty &&
      is.data.frame(src_df) && col %in% names(src_df) &&
      is.logical(src_df[[col]]) && real %in% c("TRUE", "FALSE")) {
    return(
      if (identical(real, "TRUE")) {
        bquote(.(sym) %in% TRUE)
      } else {
        bquote(!(.(sym) %in% TRUE))
      }
    )
  }

  parts <- list()

  if (length(real) > 0L) {
    casted <- real
    if (is.data.frame(src_df) && col %in% names(src_df)) {
      src <- src_df[[col]]
      if (is.integer(src)) {
        intv <- suppressWarnings(as.integer(real))
        if (!any(is.na(intv))) casted <- intv
      } else if (is.numeric(src)) {
        num <- suppressWarnings(as.numeric(real))
        if (!any(is.na(num))) casted <- num
      } else if (is.logical(src)) {
        bool <- as.logical(real)
        if (!any(is.na(bool))) casted <- bool
      }
    }
    parts <- c(parts, list(bquote(.(sym) %in% .(casted))))
  }
  if (has_na) parts <- c(parts, list(bquote(is.na(.(sym)))))
  if (has_empty) parts <- c(parts, list(bquote(.(sym) == "")))

  if (length(parts) == 0L) return(NULL)
  # Build the OR tree explicitly; a single part stays the bare expression so
  # the no-token path is byte-identical to before.
  Reduce(function(a, b) bquote(.(a) | .(b)), parts)
}

#' Combine N condition expressions with `&` or `|`.
#' @noRd
combine_conds <- function(exprs, operator = "&") {
  if (!identical(operator, "|")) operator <- "&"
  combined <- exprs[[1L]]
  if (length(exprs) > 1L) {
    for (i in seq.int(2L, length(exprs))) {
      combined <- call(operator, combined, exprs[[i]])
    }
  }
  combined
}

#' Normalize state for JSON transport.
#'
#' Per-column-entry `values` need `as.list()` so length-1 vectors survive
#' `toJSON(auto_unbox = TRUE)`. The outer `columns` list survives as-is.
#' @noRd
normalize_state_for_json <- function(s) {
  if (is.null(s)) s <- list(columns = list())
  op <- s$operator %||% "&"
  if (!identical(op, "|")) op <- "&"
  cols <- s$columns %||% list()
  cols_norm <- lapply(cols, function(e) {
    out <- list(
      name   = if (is.null(e$name)) "" else as.character(e$name)[1L],
      mode   = e$mode %||% "single",
      values = as.list(as.character(e$values %||% character()))
    )
    if (!is.null(e$table) && nzchar(e$table)) out$table <- e$table
    out
  })
  list(columns = cols_norm, operator = op)
}

# The preview container. Every branch of block_output.value_filter_block
# below returns a renderUI (dm diagram, dynamic table, HTML fallback), so the
# container MUST be a uiOutput. Without this method the block fell back to
# block_ui.transform_block's DT::dataTableOutput: the client then binds the
# element as an htmlwidget, and the arriving `{html, deps}` renderUI payload
# crashes htmlwidgets' renderValue (`data.evals` undefined -> TypeError),
# killing the whole Shiny message batch it rides in -- every other output in
# that flush (other blocks' previews, titles, downstream re-renders) is
# silently lost. Same pairing as block_ui.dm_block / block_ui.crossfilter_block.
#' @method block_ui value_filter_block
#' @export
block_ui.value_filter_block <- function(id, x, ...) {
  shiny::tagList(
    shiny::uiOutput(shiny::NS(id, "result"))
  )
}

# Render the block output preview. The block accepts a data frame or a `dm`, so
# the output renderer dispatches on the result type (same pattern as
# block_output.crossfilter_block): a `dm` routes to block_output.dm_block (the
# interactive diagram + click-to-preview); a data frame uses blockr.extra's
# dynamic renderer (paginated HTML table) when available, otherwise a plain HTML
# table fallback. Internal S3 method -- registered, not separately documented.
#' @method block_output value_filter_block
#' @export
block_output.value_filter_block <- function(x, result, session) {
  if (inherits(result, "dm")) {
    return(block_output.dm_block(x, result, session))
  }
  # Data frame or other: use blockr.extra's dynamic renderer if available,
  # otherwise a simple HTML table preview.
  render_fn <- tryCatch(
    get("render_dynamic_output", asNamespace("blockr.extra")),
    error = function(e) NULL
  )
  if (!is.null(render_fn)) {
    render_fn(result, x, session)
  } else {
    shiny::renderUI({
      if (!is.data.frame(result)) return(NULL)
      dat <- utils::head(result, 100)
      shiny::tags$div(
        style = "max-height: 400px; overflow: auto;",
        shiny::tags$table(
          class = "table table-sm table-striped",
          shiny::tags$thead(shiny::tags$tr(
            lapply(names(dat), function(n) shiny::tags$th(n))
          )),
          shiny::tags$tbody(
            lapply(seq_len(nrow(dat)), function(i) {
              shiny::tags$tr(lapply(dat[i, ], function(v) {
                shiny::tags$td(as.character(v))
              }))
            })
          )
        )
      )
    })
  }
}

# Custom render trigger for the value filter block. Mirrors
# block_render_trigger.crossfilter_block / the dm_block override: the default
# transform-block trigger reaches for pagination board options that aren't set
# for this block, which leaves the dm diagram / table preview unrendered. The
# block manages its own output, so no extra trigger is needed. Internal S3
# method -- registered, not separately documented.
#' @method block_render_trigger value_filter_block
#' @export
block_render_trigger.value_filter_block <- function(
  x,
  session = blockr.core::get_session()
) {
  NULL
}

#' HTML dependency for the value filter block JS + CSS
#' @noRd
value_filter_block_dep <- memoise0(function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "blockr-value-filter-js",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("js", package = "blockr.dm"),
      script = "value-filter-block.js"
    ),
    htmltools::htmlDependency(
      name = "blockr-value-filter-css",
      version = utils::packageVersion("blockr.dm"),
      src = system.file("css", package = "blockr.dm"),
      stylesheet = "value-filter-block.css"
    )
  )
})
