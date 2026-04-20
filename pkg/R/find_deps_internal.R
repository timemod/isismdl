#' Internal version of find_deps for IsisMdl objects.
#'
#' @param mdl The \code{IsisMdl} object.
#' @param final_destinations A data frame with variable names and periods of
#' the final destinations.
#' @param final_sources A data frame with variable names and periods of the
#' final sources.
#' @param solvable A data frame with variable names and periods of values that
#' we wish to solve.
#' @param observed_data A data frame containing observed values. If NULL,
#' the model data is used.
#' @param ignore_observed Logical, should observed variables be ignored?
#' @param stop_if_final_dest_observed Logical, should the search stop if a
#' final destination is already observed?
#' @return A dependency graph as an igraph object.
#'
#' @importFrom igraph add_edges make_empty_graph neighbors induced_subgraph
#'   delete_edges incident subcomponent delete_vertices
#'   add_vertices V degree set_vertex_attr
#' @importFrom dplyr rename mutate anti_join
#' @importFrom rlang .data
#'
#' @noRd
find_deps_internal <- function(mdl, final_destinations, final_sources,
                               solvable, observed_data, ignore_observed,
                               stop_if_final_dest_observed) {

  dep_struc <- mdl$get_dep_struct(one_lag_per_row = TRUE)

  return(find_deps(final_destinations = final_destinations,
                   final_sources = final_sources,
                   solvable = solvable,
                   observed_data = observed_data,
                   dep_struc = dep_struc,
                   mdl = mdl,
                   ignore_observed = ignore_observed,
                   stop_if_final_dest_observed = stop_if_final_dest_observed))
}
