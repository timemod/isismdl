#' @title Convert a dependency graph to a data frame
#'
#' @description
#' This utility function takes a directed graph returned by
#' \code{\link{find_deps}} and converts its vertex attributes into a clean,
#' tabular format. It parses the vertex names (e.g. \code{"var[period]"}) into
#' separate \code{var} and \code{period} columns. For an example,
#' see [find_deps].
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
    split_vertex_name_column()
}
