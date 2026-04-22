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
