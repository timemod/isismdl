#' Utility functions for var_period class and dependency analysis.
#'
#' This file contains shared utility functions used in dependency search
#' logic, primarily for use in find_deps.
#'
#' Technical Purpose and Usage:
#' The var_period class provides a structured way to represent a model variable
#' at a specific period. Its S3 methods (as.character and print) ensure these
#' objects are correctly represented as character strings like "var[period]"
#' when used as vertex names in igraph dependency graphs.
#'
#' During the recursive dependency search in find_deps, these utilities allow
#' for efficient handling, parsing, and creation of nodes. After the search
#' is complete, helpers like as_data_frame_deps allow the final graph to be
#' converted back into a clean tabular format by splitting the character
#' vertex names back into their variable and period components.
#'
#' @noRd

#' @importFrom regts as.period
#' @importFrom stringr str_match
#' @importFrom tibble remove_rownames
#' @importFrom tidyr separate

new_var_period <- function(var, period) {
  period <- regts::as.period(period)
  structure(list(var = var, period = period), class = "var_period")
}

#' @keywords internal
#' @exportS3Method as.character var_period
as.character.var_period <- function(x, ...) {
  paste0(x$var, "[", x$period, "]")
}

#' @keywords internal
#' @exportS3Method print var_period
print.var_period <- function(x, ...) {
  print(as.character(x))
}

as_var_period <- function(txt) {
  pattern <- "^([a-zA-Z][a-zA-Z_]*)\\[(\\d+)\\]$"
  x <- stringr::str_match(txt, pattern)
  new_var_period(x[, 2], x[, 3])
}

# Split the column 'name' of a data frame into columns 'var' and 'period'.
# Column 'name' should contain vertex names, such as "y1[2025]".
split_vertex_name_column <- function(df) {
  tidyr::separate(
    df,
    "name",
    into = c("var", "period"),
    sep = "\\[|\\]",
    extra = "drop"
  )
}

dep_struct_to_list <- function(dep_struc) {
  if (is.list(dep_struc) && !is.data.frame(dep_struc)) {
    return(dep_struc)
  }
  lhs <- dep_struc$lhs
  dep_struc |>
    dplyr::rename(var = "rhs", period = "lag") |>
    dplyr::select(-"lhs") |>
    split(lhs)
}

to_vertex_names <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(character(0))
  }
  paste0(df$var, "[", toupper(as.character(df$period)), "]")
}

# Convert a character vector with vertex names (e.g. "y[2025]")
# to a data frame with columns 'var' and 'period'.
vertex_names_to_df <- function(vertex_names) {
  split_vertex_name_column(data.frame(name = vertex_names))
}

check_endo <- function(vars, endo_names, label) {
  if (is.null(vars) || length(vars) == 0) {
    return()
  }
  no_endo <- setdiff(vars, endo_names)
  if (length(no_endo) > 0) {
    stop(sprintf("The following %s variables are not endogenous model variables:\n%s.",
                 label, paste(no_endo, collapse = ", ")))
  }
}

check_overlap <- function(v1, v2, msg) {
  problem_vertices <- intersect(v1, v2)
  if (length(problem_vertices) > 0) {
    stop(sprintf("%s:\n%s", msg, paste(problem_vertices, collapse = ", ")))
  }
}

check_unique <- function(v, label) {
  if (any(duplicated(v))) {
    v_dups <- unique(v[duplicated(v)])
    stop(
      sprintf(
        "The following %s variables are specified more than once for the same period:\n%s.",
        label, paste(v_dups, collapse = ", ")
      )
    )
  }
}

check_var_period_df <- function(df, label, check_value = FALSE) {
  if (is.null(df)) {
    return()
  }
  if (!is.data.frame(df)) {
    stop(sprintf("%s must be a data frame.", label))
  }
  if (!all(c("var", "period") %in% names(df))) {
    stop(sprintf("%s must have columns 'var' and 'period'.", label))
  }
  if (nrow(df) == 0) {
    return()
  }

  # Check if periods are valid regts periods
  tryCatch({
    regts::as.period(df$period)
  }, error = function(e) {
    stop(sprintf("Invalid periods in %s: %s", label, e$message))
  })

  if (check_value) {
    if (!"value" %in% names(df)) {
      stop(sprintf("%s must have a 'value' column.", label))
    }
    if (!is.numeric(df$value)) {
      stop(sprintf("Column 'value' in %s must be numeric.", label))
    }
    if (any(is.na(df$value))) {
      stop(sprintf("Column 'value' in %s contains NAs.", label))
    }
  }

  v_names <- to_vertex_names(df)
  check_unique(v_names, label)
}
