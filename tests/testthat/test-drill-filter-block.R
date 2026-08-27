# A CDISC-shaped dm: USUBJID is in every table and is the PK on adsl, which is
# the case that decides `claim_table_for()`'s tie-break.
drill_test_dm <- function() {
  adsl <- data.frame(
    USUBJID = c("1", "2", "3"),
    SEX = c("F", "M", "F"),
    DTHFL = c("Y", "N", "N"),
    stringsAsFactors = FALSE
  )
  adae <- data.frame(
    USUBJID = c("1", "1", "2"),
    AEDECOD = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )
  d <- dm::dm(adsl = adsl, adae = adae)
  d <- dm::dm_add_pk(d, adsl, USUBJID)
  dm::dm_add_fk(d, adae, USUBJID, adsl)
}

claim <- function(...) {
  list(columns = list(list(...)))
}

# Same roundtrip test-serdes.R uses; helpers there are file-local, so this one
# carries its own copy.
ser_deser <- function(block) {
  json <- jsonlite::toJSON(
    blockr_ser(block), null = "null", auto_unbox = TRUE
  )
  blockr_deser(
    jsonlite::fromJSON(
      as.character(json),
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    )
  )
}

test_that("the drill filter is a value filter with its own class", {

  blk <- new_drill_filter_block()

  expect_s3_class(blk, "drill_filter_block")
  # The S3 methods (block_ui / block_output / block_render_trigger) are defined
  # on value_filter_block and must keep dispatching.
  expect_s3_class(blk, "value_filter_block")
  expect_s3_class(blk, "transform_block")
})

test_that("the drill filter serializes as ITSELF, not as a value filter", {

  # The trap the two constructors exist to avoid: `new_transform_block()` takes
  # the ctor from `sys.parent()`, so a drill filter implemented as a wrapper
  # around `new_value_filter_block()` would come back from a saved board as a
  # plain value filter, silently losing the drill target.
  ct <- attributes(attr(new_drill_filter_block(), "ctor"))

  expect_identical(ct$fun, "new_drill_filter_block")
  expect_identical(ct$pkg, "blockr.dm")

  blk <- new_drill_filter_block(
    state = claim(name = "SEX", table = "adsl", mode = "multi", values = "F")
  )
  restored <- ser_deser(blk)

  expect_s3_class(restored, "drill_filter_block")
  expect_equal(
    blockr.core:::initial_block_state(restored)$state,
    blockr.core:::initial_block_state(blk)$state
  )
})

test_that("a claim naming one table's column resolves to that table", {

  shape <- filter_input_shape(drill_test_dm())
  res <- resolve_claim_tables(
    claim(name = "SEX", mode = "multi", values = "F"), shape
  )

  expect_identical(res$state$columns[[1L]]$table, "adsl")
  expect_length(res$unresolved, 0L)
})

test_that("a claim on a column several tables carry goes to the PK table", {

  # USUBJID is in adsl AND adae. dm_filter() cascades along foreign keys, so
  # filtering the parent narrows the children too; filtering adae would leave
  # adsl whole and the cohort wrong.
  shape <- filter_input_shape(drill_test_dm())
  res <- resolve_claim_tables(
    claim(name = "USUBJID", mode = "multi", values = c("1", "2")), shape
  )

  expect_identical(res$state$columns[[1L]]$table, "adsl")
  expect_length(res$unresolved, 0L)
})

test_that("a derived column no table carries is reported, not silently dropped", {

  # The composer disposition case: the block builds DEATH out of DTHFL, so the
  # claim names a column that exists only inside the sender.
  shape <- filter_input_shape(drill_test_dm())
  res <- resolve_claim_tables(
    claim(name = "DEATH", mode = "multi", values = "Died"), shape
  )

  expect_identical(res$unresolved, "DEATH")
  expect_null(res$state$columns[[1L]]$table)

  # And it filters nothing rather than erroring or filtering the wrong table.
  expect_equal(
    make_filter_expr_from_shape(res$state$columns, shape),
    make_filter_expr_from_shape(list(), shape)
  )
})

test_that("an entry that already names a valid table is left alone", {

  # A sender that does set `ctrl_table` keeps working unchanged.
  shape <- filter_input_shape(drill_test_dm())
  st <- claim(name = "USUBJID", table = "adae", mode = "multi", values = "1")
  res <- resolve_claim_tables(st, shape)

  expect_identical(res$state$columns[[1L]]$table, "adae")
  expect_identical(res$state, st)
})

test_that("a table name the dm does not have is re-resolved", {

  shape <- filter_input_shape(drill_test_dm())
  res <- resolve_claim_tables(
    claim(name = "SEX", table = "advs", mode = "multi", values = "F"), shape
  )

  expect_identical(res$state$columns[[1L]]$table, "adsl")
})

test_that("resolution is idempotent", {

  # Load-bearing: the server observer both reads and writes `r_state`, and
  # stops because the second pass is identical(). A non-idempotent resolve
  # would loop for ever, taking a board update with it each time.
  shape <- filter_input_shape(drill_test_dm())
  once <- resolve_claim_tables(
    claim(name = "SEX", mode = "multi", values = "F"), shape
  )
  twice <- resolve_claim_tables(once$state, shape)

  expect_identical(once$state, twice$state)
  expect_identical(once$unresolved, twice$unresolved)
})

test_that("a resolved claim builds the dm_filter expression it should", {

  shape <- filter_input_shape(drill_test_dm())
  res <- resolve_claim_tables(
    claim(name = "SEX", mode = "multi", values = "F"), shape
  )

  expect_equal(
    make_filter_expr_from_shape(res$state$columns, shape),
    make_filter_expr_from_shape(
      claim(name = "SEX", table = "adsl", mode = "multi", values = "F")$columns,
      shape
    )
  )
})

test_that("a data frame input resolves nothing and reports nothing", {

  shape <- filter_input_shape(datasets::iris)
  st <- claim(name = "Species", mode = "multi", values = "setosa")
  res <- resolve_claim_tables(st, shape)

  expect_identical(res$state, st)
  expect_length(res$unresolved, 0L)
})

test_that("an empty claim is a no-op", {

  shape <- filter_input_shape(drill_test_dm())
  st <- list(columns = list())
  res <- resolve_claim_tables(st, shape)

  expect_identical(res$state, st)
  expect_length(res$unresolved, 0L)
})

test_that("the dm's primary keys ride along on the input shape", {

  shape <- filter_input_shape(drill_test_dm())

  expect_true(shape$is_dm)
  expect_identical(shape$pks, list(adsl = "USUBJID"))

  # A keyless dm is a normal input, not a failure.
  bare <- filter_input_shape(dm::dm(a = data.frame(x = 1)))
  expect_identical(bare$pks, list())
})

test_that("the unplaceable-claim note renders only when there is one", {

  expect_null(drill_note_ui(character()))
  expect_null(drill_note_ui(NULL))

  note <- as.character(drill_note_ui("DEATH"))
  expect_match(note, "DEATH")
  expect_match(note, "that column")

  many <- as.character(drill_note_ui(c("DEATH", "AGEGRP")))
  expect_match(many, "DEATH, AGEGRP")
  expect_match(many, "those columns")
})

test_that("the value filter is unchanged by the refactor", {

  blk <- new_value_filter_block()

  expect_s3_class(blk, "value_filter_block")
  expect_false(inherits(blk, "drill_filter_block"))
  expect_identical(
    attributes(attr(blk, "ctor"))$fun, "new_value_filter_block"
  )
})

# --- the server, driven the way the control channel drives it ----------------
#
# The helper tests above prove the resolution rule. These prove the WIRING: a
# claim pushed into the block's state (which is what `ctrl_send()` does over the
# board's control channel) is resolved by the running module and reaches the
# expression. That is the step that was missing end to end.

test_that("the drill filter's state is externally controllable", {
  expect_setequal(
    blockr.core::external_ctrl_vars(new_drill_filter_block()),
    c("state", "block_name")
  )
})

test_that("the server resolves a pushed claim and filters the dm on it", {

  block <- new_drill_filter_block()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      expect_equal(nrow(dm::dm_get_tables(session$returned$result())$adsl), 3L)

      # What a sender pushes: a column and its values, and NO table -- it holds
      # no dm and cannot name one.
      session$returned$state$state(
        list(columns = list(list(name = "SEX", mode = "multi", values = "F")))
      )
      session$flushReact()

      out <- dm::dm_get_tables(session$returned$result())
      expect_equal(nrow(out$adsl), 2L)
      # and the FK cascade narrowed the child table too
      expect_equal(nrow(out$adae), 2L)

      # The block filled the table in, server-side, and kept it in its state.
      expect_identical(
        session$returned$state$state()$columns[[1L]]$table, "adsl"
      )
    },
    args = list(x = block, data = list(data = function() drill_test_dm()))
  )
})

test_that("the SAME claim into a plain value filter does nothing", {

  # The bug this block exists to fix: `make_dm_filter_expr()` skips an entry
  # with no table, so the claim is dropped and the board does not move.
  block <- new_value_filter_block()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      session$returned$state$state(
        list(columns = list(list(name = "SEX", mode = "multi", values = "F")))
      )
      session$flushReact()

      expect_equal(nrow(dm::dm_get_tables(session$returned$result())$adsl), 3L)
    },
    args = list(x = block, data = list(data = function() drill_test_dm()))
  )
})

test_that("a claim on a derived column filters nothing and is reported", {

  block <- new_drill_filter_block()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      session$returned$state$state(
        list(columns = list(list(name = "DEATH", mode = "multi", values = "Died")))
      )
      session$flushReact()

      # Unfiltered, deliberately: a claim that cannot be placed must not be
      # guessed at. The note is what tells the board author why.
      expect_equal(nrow(dm::dm_get_tables(session$returned$result())$adsl), 3L)
      # The note itself renders in the block's own (nested) module session,
      # which testServer cannot reach from here; `drill_note_ui()` is covered
      # directly above.
    },
    args = list(x = block, data = list(data = function() drill_test_dm()))
  )
})

test_that("resolving a claim does not loop", {

  # The observer both reads and writes `r_state`. If resolution were not
  # idempotent this would never settle -- and each pass would be a board update
  # on a real board.
  block <- new_drill_filter_block()

  shiny::testServer(
    blockr.core:::get_s3_method("block_server", block),
    {
      session$flushReact()
      session$returned$state$state(
        list(columns = list(list(name = "USUBJID", mode = "multi", values = "1")))
      )

      for (i in 1:5) session$flushReact()
      settled <- session$returned$state$state()

      for (i in 1:5) session$flushReact()
      expect_identical(session$returned$state$state(), settled)
      expect_identical(settled$columns[[1L]]$table, "adsl")
    },
    args = list(x = block, data = list(data = function() drill_test_dm()))
  )
})
