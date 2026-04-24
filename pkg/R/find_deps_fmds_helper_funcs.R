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

#' @title Convert a dependency graph to a data frame
#'
#' @description
#' This utility function takes a directed graph returned by
#' \code{\link{find_deps}} and converts its vertex attributes into a clean,
#' tabular format. It parses the vertex names (e.g. \code{"var[period]"}) into
#' separate \code{var} and \code{period} columns.
#'
#' @param g A directed \code{igraph} object, typically the result of
#'   calling \code{find_deps}.
#'
#' @return A \code{tibble} (data frame) where each row represents a vertex
#'   from the graph. All vertex attributes (such as \code{ok}, \code{observed},
#'   \code{calculable}, etc.) are included as columns.
#'
#' @seealso \code{\link{find_deps}}
#'
#' @export
as_data_frame_deps <- function(g) {
  igraph::as_data_frame(g, what = "vertices") |>
    tibble::remove_rownames() |>
    tidyr::separate(
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
  paste0(df$var, "[", as.character(df$period), "]")
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
    dups <- unique(v[duplicated(v)])
    stop(sprintf("The following %s variables are specified more than once for the same period:\n%s.",
                 label, paste(dups, collapse = ", ")))
  }
}
