# The receiving end of the cross-block control channel.
#
# blockr.viz's bridge extension (`new_ctrl_bridge_extension()`) opens the
# channel; a drilling block (a chart, a table, a tile, or a composer table
# drawn straight in a function block) pushes a CLAIM down it -- "the user
# clicked SEX = F". This block is what the claim lands in.
#
# It is a value filter with two differences, and both exist because the sender
# does not hold the data model:
#
# 1. IT IS FOUND BY CLASS, not by counting. `ctrl_targets("drill_filter_block")`
#    returns the one block on the board that is the drill's destination, so a
#    sender needs no configured target. That is the whole reason this is a
#    separate class rather than an argument on the value filter: a board
#    typically carries many value filters (the CDEx 244 board carries eight),
#    and a sender scanning for them cannot tell which one means "the cohort the
#    user just drilled into". blockr.viz's senders fall back to the value
#    filter when no drill filter is present, so existing boards are unaffected.
#
# 2. IT RESOLVES THE TABLE ITSELF. A claim names a column. On a `dm` a
#    condition must also name the table it applies to -- `make_dm_filter_expr()`
#    skips any entry whose table is empty -- and the sender cannot supply it
#    without the dm. Senders in blockr.viz work around that with a `ctrl_table`
#    argument the board author sets by hand; the composer drill has no gear to
#    set it in and sends `table = ""`, which is why its claims were dropped
#    without a trace. Here the dm is in hand, so the lookup happens where the
#    answer is, and `ctrl_table` becomes optional.
#
# A claim that resolves to no table at all is REPORTED, not swallowed. That is
# the derived-column case: a composer table that builds `DEATH` out of `DTHFL`
# claims `DEATH`, and no table in the dm has that column. Before this block the
# claim went out, the sender's status line said "Filtered: DEATH = Died", and
# nothing moved. The fix for that stays the author's (stamp `source_data` so
# the click resolves to subject ids instead), but the board now says which
# claim it could not place instead of leaving it to be discovered by eye.

#' Drill filter block
#'
#' The destination for cross-block drill claims: a [new_value_filter_block()]
#' that senders can find without being told, and that resolves a claimed
#' column to a table in its own `dm`.
#'
#' Put one on a board together with `blockr.viz::new_ctrl_bridge_extension()`.
#' The two go together: the bridge opens the control channel and this block is
#' what the channel writes into, so a board with a bridge and no drill filter
#' has senders with nowhere to send, and a drill filter without a bridge is
#' just a value filter.
#'
#' @section One per board:
#' A sender resolves its target by class and takes it only when there is
#' exactly one candidate, so a second drill filter turns the drill back off
#' rather than picking a winner. Use plain value filter blocks for every other
#' filtering job on the board; they are not candidates and do not interfere.
#'
#' @section What it does with a claim:
#' A claim is a list of `list(name=, mode=, values=)` entries pushed into the
#' block's `state` over the control channel. On a `dm` input each entry needs a
#' `table` as well, and this block fills it in:
#'
#' * exactly one table carries the column: that table.
#' * several carry it (a subject id is in every ADaM table): the table where it
#'   is the PRIMARY KEY, because `dm::dm_filter()` cascades along foreign keys,
#'   so restricting the parent restricts the children too.
#' * no table carries it: nothing is filtered and the block says which column
#'   it could not place. This means the sender claimed a column it DERIVED, and
#'   the fix belongs at the sender.
#'
#' An entry that already names a valid table is left alone, so a sender that
#' does set `ctrl_table` keeps working unchanged.
#'
#' On a plain data frame input there are no tables to resolve and this block
#' behaves exactly like a value filter.
#'
#' @inheritParams new_value_filter_block
#'
#' @return A block object.
#'
#' @examples
#' if (interactive()) {
#'   library(blockr.core)
#'   library(blockr.dm)
#'   serve(
#'     new_drill_filter_block(),
#'     data = list(data = datasets::iris)
#'   )
#' }
#'
#' @export
new_drill_filter_block <- function(
  state = list(columns = list()),
  ...
) {
  blockr.core::new_transform_block(
    value_filter_server(migrate_value_filter_state(state), drill = TRUE),
    value_filter_ui(drill = TRUE),
    dat_valid = value_filter_dat_valid,
    # The value filter class is kept in the vector deliberately: the S3 methods
    # (`block_ui`, `block_output`, `block_render_trigger`) dispatch on it, and a
    # sender offering a picker of value filters should list this block too.
    class = c("drill_filter_block", "value_filter_block"),
    expr_type = "bquoted",
    external_ctrl = TRUE,
    allow_empty_state = "state",
    ...
  )
}

# --- claim resolution --------------------------------------------------------

#' Fill in the `table` of every claim entry that lacks one.
#'
#' Idempotent, and it must be: the observer calling it both reads and writes
#' `r_state`, and relies on a second pass being `identical()` to stop.
#'
#' @param state The filter state.
#' @param shape A `filter_input_shape()`: `tables` (0-row templates) and `pks`.
#' @return `list(state=, unresolved=)`, `unresolved` being the claimed column
#'   names no table carries.
#' @noRd
resolve_claim_tables <- function(state, shape) {

  cols <- state$columns %||% list()

  if (!length(cols) || !isTRUE(shape$is_dm)) {
    return(list(state = state, unresolved = character()))
  }

  tbls <- shape$tables %||% list()
  pks <- shape$pks %||% list()
  unresolved <- character()

  for (i in seq_along(cols)) {

    entry <- cols[[i]]
    tbl <- entry$table %||% ""

    # Already placed -- by the sender's `ctrl_table`, or by a previous pass.
    if (nzchar(tbl) && tbl %in% names(tbls)) {
      next
    }

    hit <- claim_table_for(entry$name, tbls, pks)

    if (is.null(hit)) {
      unresolved <- c(unresolved, as.character(entry$name))
      next
    }

    cols[[i]]$table <- hit
  }

  state$columns <- cols

  list(state = state, unresolved = unresolved)
}

#' Which table a claimed column belongs to.
#'
#' `NULL` when no table carries it. With several candidates the PRIMARY KEY
#' table wins: `dm::dm_filter()` cascades along foreign keys, so restricting the
#' parent restricts every child, whereas restricting one child leaves its
#' siblings untouched. A subject id claimed off an AE table should narrow the
#' whole data model, not just the AEs.
#' @noRd
claim_table_for <- function(col, tbls, pks) {

  if (!is.character(col) || length(col) != 1L || is.na(col) || !nzchar(col)) {
    return(NULL)
  }

  hits <- names(tbls)[vapply(
    tbls, function(t) col %in% names(t), logical(1L)
  )]

  if (!length(hits)) {
    return(NULL)
  }

  if (length(hits) == 1L) {
    return(hits[[1L]])
  }

  pk_hit <- hits[vapply(
    hits, function(h) col %in% (pks[[h]] %||% character()), logical(1L)
  )]

  if (length(pk_hit)) {
    return(pk_hit[[1L]])
  }

  hits[[1L]]
}

#' Table -> primary key columns, as a plain named list.
#'
#' Metadata the dm already holds, so this costs no query. Wrapped because a dm
#' with no keys set at all is a normal input, not a failure.
#' @noRd
dm_pk_map <- function(dm_obj) {

  pks <- tryCatch(dm::dm_get_all_pks(dm_obj), error = function(e) NULL)

  if (is.null(pks) || !nrow(pks)) {
    return(list())
  }

  stats::setNames(
    lapply(pks$pk_col, as.character),
    as.character(pks$table)
  )
}

#' The unplaceable-claim line.
#'
#' Nothing to say when everything resolved, so the slot renders empty and the
#' block looks exactly like a value filter until a claim actually fails.
#' @noRd
drill_note_ui <- function(unresolved) {

  if (!length(unresolved)) {
    return(NULL)
  }

  htmltools::div(
    class = "drill-filter-note",
    htmltools::span(
      class = "drill-filter-note-head",
      paste0(
        "Not filtered: ",
        paste(unique(unresolved), collapse = ", ")
      )
    ),
    htmltools::span(
      class = "drill-filter-note-hint",
      if (length(unique(unresolved)) == 1L) {
        "No table in this data model has that column."
      } else {
        "No table in this data model has those columns."
      }
    )
  )
}
