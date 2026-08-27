#' Register dm Blocks
#'
#' Registers the dm blocks with blockr.
#'
#' @export
#' @importFrom blockr.core register_blocks new_arg_specs new_arg_spec
#'   arg_string arg_number arg_integer arg_boolean arg_enum arg_array arg_object
register_dm_blocks <- function() {
  blockr.core::register_blocks(
    c(
      "new_dm_read_block",
      "new_dm_write_block",
      "new_dm_block",
      "new_cdisc_dm_block",
      "new_dm_select_block",
      "new_dm_add_keys_block",
      "new_dm_filter_block",
      "new_dm_filter_by_data_block",
      "new_dm_pull_block",
      "new_dm_flatten_block",
      "new_temporal_join_block",
      "new_dm_temporal_join_block",
      "new_crossfilter_block",
      "new_dm_example_block",
      "new_value_filter_block",
      "new_drill_filter_block"
    ),
    name = c(
      "Import multiple tables",
      "Write dm",
      "Create dm",
      "CDISC dm",
      "Select tables",
      "Add keys",
      "Filter dm",
      "Filter dm by data",
      "Pull table",
      "Flatten dm",
      "Temporal join",
      "dm Temporal join",
      "Crossfilter",
      "Example data model",
      "Value filter",
      "Drill filter"
    ),
    description = c(
      paste(
        "Read a whole directory, ZIP archive or Excel workbook at once:",
        "one table per file or sheet, linked as a dm"
      ),
      "Write dm object to Excel, ZIP, or directory",
      "Combine data frames into a dm (data model) object",
      paste(
        "Set CDISC keys (USUBJID PK/FK) and",
        "optionally deduplicate subject columns"
      ),
      "Select a subset of tables to keep in a dm",
      "Add primary and foreign key relationships to dm",
      paste(
        "Filter dm by condition in any table,",
        "cascading to related tables via FKs"
      ),
      paste(
        "Semi-join a dm to ids supplied by a second data frame input,",
        "cascading via FKs"
      ),
      "Extract a single table from dm as a data frame",
      "Flatten dm into a single data frame by joining",
      paste(
        "Join two tables and filter by time window",
        "between date columns"
      ),
      paste(
        "Temporal join between two dm tables,",
        "filtering by time window"
      ),
      paste(
        "Client-side crossfilter for data frames",
        "and dm objects using crossfilter2.js"
      ),
      "Load a pre-built dm from example datasets",
      paste(
        "Minimal value filter for a data frame or dm.",
        "Columns hidden behind the gear; per-column single vs multi",
        "select. dm input cascades via FKs."
      ),
      paste(
        "The value filter a drill-down click writes into. One per board,",
        "beside blockr.viz's control bridge extension; senders find it by",
        "class and it resolves a claimed column to a dm table itself."
      )
    ),
    category = c(
      # A block that fetches data is "input", whatever shape it hands back,
      # and one that writes it is "output": those are the shelves people
      # look on for "how do I get my files in" and "how do I get them out".
      # "structured" is where you look for things that reshape a dm you
      # already have.
      "input",
      "output",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "structured",
      "input",
      "structured",
      "structured"
    ),
    icon = c(
      "file-earmark-arrow-up",
      "file-earmark-arrow-down",
      "diagram-3",
      "patch-check",
      "check2-square",
      "key",
      "funnel",
      "link-45deg",
      "box-arrow-up-right",
      "layers",
      "clock-history",
      "clock-history",
      "lightning",
      "database",
      "filter",
      "crosshair"
    ),
    guidance = c(
      # new_dm_read_block:
      paste(
        "Reads tabular data from disk into a dm (data model) object. Use as",
        "the entry point when working with multi-table datasets from Excel /",
        "ZIP / directories. Which file formats a directory or ZIP may hold",
        "comes from blockr.io's format registry, so formats registered by",
        "other loaded packages work here too, and one folder may mix",
        "formats. `args` is a named list of reader options applied uniformly",
        "to every member (each format takes what it understands), e.g.",
        "`args = list(sep = \";\")` for a folder of semicolon CSVs. The",
        "output is a dm that downstream dm_* blocks can filter, select,",
        "join, and flatten."
      ),
      # new_dm_write_block:
      paste(
        "Write a dm object back to disk in Excel / CSV / Parquet form.",
        "Use as a terminal block to export a curated dm."
      ),
      # new_dm_block:
      paste(
        "Wraps a set of data frames into a dm (data model) object. Use when",
        "the upstream already provides multiple data frames \u2014 typically via",
        "bind_cols or multiple file loads \u2014 and you want to treat them as a",
        "related table collection. For CDISC/ADaM data prefer cdisc_dm_block",
        "which knows the USUBJID convention."
      ),
      # new_cdisc_dm_block:
      paste(
        "Turns a plain dm of CDISC/ADaM tables (adsl, adae, adlbc, ...) into",
        "a keyed dm where USUBJID is the primary key on ADSL and the foreign",
        "key on every other table. Always place this block immediately after",
        "loading CDISC data and before any filtering, joining, or crossfilter",
        "work. Without keys, downstream dm blocks can't cascade filters."
      ),
      # new_dm_select_block:
      paste(
        "Narrows a dm to a subset of its tables. Use to trim large dm objects",
        "before flattening or displaying."
      ),
      # new_dm_add_keys_block:
      paste(
        "Explicitly sets a primary/foreign key relationship on a dm. Use when",
        "`dm_block(infer_keys=FALSE)` was used or when the relationships need",
        "manual correction. For CDISC data, prefer cdisc_dm_block which sets",
        "these automatically."
      ),
      # new_dm_filter_block:
      paste(
        "Filter a dm by one or more conditions on ONE selected table;",
        "matching rows cascade to all related tables via the dm's FK",
        "relationships. Reuses blockr.dplyr's type-aware filter UI",
        "(categorical multi-select, numeric operator+value, or free R",
        "expression) combined with & or |.",
        "\n\nDo NOT use blockr.dplyr::filter_block on a dm - it doesn't",
        "understand cascading."
      ),
      # new_dm_filter_by_data_block:
      paste(
        "Filters a dm by matching rows in a secondary data frame input (`by`).",
        "Use to bridge drill-down / table outputs (data frames) back into a",
        "dm: e.g. click a patient on a trajectory chart, feed the resulting",
        "data frame in via `by`, and all downstream dm consumers (patient",
        "profile, flatten, summaries) see the restricted dm.",
        "\n\nThe block has TWO inputs: \"data\" (the dm) and \"by\" (the",
        "filtering data frame). dm::dm_filter cascades via FKs, so setting",
        "`table = \"adsl\"` with `key_col = \"USUBJID\"` restricts every",
        "related table in one step. Any table/column combination that exists",
        "in both inputs works."
      ),
      # new_dm_pull_block:
      paste(
        "Extracts a single table from a dm as a plain data frame, dropping",
        "the dm wrapper. Use when a downstream block only accepts a data",
        "frame (e.g. blockr.dplyr blocks, most plot blocks) but the upstream",
        "is a dm."
      ),
      # new_dm_flatten_block:
      paste(
        "Flattens a dm into a single data frame by chained joins starting",
        "from `start_table`. Use when you need a plain wide table for",
        "downstream plotting or modeling."
      ),
      # new_temporal_join_block:
      paste(
        "Plain two-table temporal join (no dm wrapper). Joins x and y on",
        "`by` AND filters to rows where y's `right_date` is within",
        "`window_days` of x's `left_date`. Use this when you have two plain",
        "data frames. For dm objects, use dm_temporal_join_block."
      ),
      # new_dm_temporal_join_block:
      paste(
        "Temporal join between two tables within a dm, filtering the right",
        "table to rows whose date falls within `window_days` of the left",
        "date. Use for \"AE on treatment\" style windows: which events occur",
        "within N days of treatment start / end / visit.",
        "\n\nMap common requests:",
        "\n- \"AEs within 30 days of treatment start\" ->",
        "left_table=\"adsl\", left_date=\"TRTSDT\", right_table=\"adae\",",
        "right_date=\"ASTDT\", window_days=30, direction=\"after\"",
        "\n- \"concomitant meds within a week before baseline\" ->",
        "left_date=\"TRTSDT\", right_date=\"CMSTDT\", window_days=7,",
        "direction=\"before\""
      ),
      # new_crossfilter_block:
      paste(
        "Interactive client-side crossfilter for data frames or dm objects.",
        "Renders a linked bar/histogram for each `active_dims` column;",
        "clicking a bar filters all downstream views. Categorical filters",
        "live in `filters`, numeric/date ranges in `range_filters`.",
        "\n\nUse whenever the user wants linked filtering across multiple",
        "views. Don't reimplement by chaining filter + join \u2014 this single",
        "block delivers the full brush/linked-filter UX."
      ),
      # new_dm_example_block:
      paste(
        "Loads a pre-built dm from the blockr.dm example catalog. Use for",
        "demos and testing \u2014 no file paths needed."
      ),
      # new_value_filter_block:
      "",
      # new_drill_filter_block:
      paste(
        "The destination for cross-block drill-down claims. Put ONE on a",
        "board together with blockr.viz::new_ctrl_bridge_extension() \u2014 the",
        "two belong together: the bridge opens the control channel, this",
        "block is what the channel writes into. A drilling chart / table /",
        "tile then needs no ctrl_target, because senders resolve it by",
        "class. Unlike a plain value filter it also fills in WHICH dm table",
        "a claimed column belongs to, and reports a claim no table can take",
        "instead of dropping it. Use plain new_value_filter_block() for",
        "every other filtering job on the board."
      )
    ),
    arguments = list(
      dm_read_arguments(),
      dm_write_arguments(),
      dm_block_arguments(),
      cdisc_dm_arguments(),
      dm_select_arguments(),
      dm_add_keys_arguments(),
      dm_filter_arguments(),
      dm_filter_by_data_arguments(),
      dm_pull_arguments(),
      dm_flatten_arguments(),
      temporal_join_arguments(),
      dm_temporal_join_arguments(),
      crossfilter_arguments(),
      dm_example_arguments(),
      NULL,
      NULL
    ),
    package = utils::packageName(),
    overwrite = TRUE
  )
}
