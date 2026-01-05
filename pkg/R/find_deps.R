#' @importFrom igraph add_edges make_empty_graph neighbors induced_subgraph
#'   delete_edges incident subcomponent delete_vertices
#'   add_vertices V degree set_vertex_attr
#' @importFrom dplyr rename mutate anti_join
#' @importFrom tibble remove_rownames
#' @importFrom tidyr separate
#' @importFrom regts as.period
#' @importFrom stringr str_match
#' @importFrom rlang .data

new_var_period <- function(var, period) {
  period <- regts::as.period(period)
  return(structure(list(var = var, period = period), class = "var_period"))
}

#' @keywords internal
#' @exportS3Method as.character var_period
as.character.var_period <- function(x, ...) {
  return(paste0(x$var, "[", x$period, "]"))
}

#' @keywords internal
#' @exportS3Method print var_period
print.var_period <- function(x, ...) {
  return(print(as.character(x)))
}

as_var_period <- function(txt) {
  pattern <- "^([a-zA-Z][a-zA-Z_]*)\\[(\\d+)\\]$"
  x <- stringr::str_match(txt, pattern)
  return(new_var_period(x[, 2], x[, 3]))
}

# Recursively find all dependencies.
# INPUT
# final_destinations
#     A data frame with variable names and periods of the final destinations.
# final_source
#     A data frame with variable names and periods of the final sources.
# solvable
#    A data frame with variable names and periods of values that we wish to
#    solve in fill_mdl_data_sove.
#    A vertex that is incalculable but only depends on either calculable vertices
#    or solvable vertices gets the status ok = TRUE.
# TODO: create nice roxygen documentation.
find_deps <- function(final_destinations, final_sources = NULL,
                      solvable = NULL,
                      observed_data, dep_struc, mdl,
                      ignore_observed = FALSE,
                      stop_if_final_dest_observed = TRUE) {

  # TODO: use a data frame with possible vertex types and the
  # corresponding color.

  # TODO: give an error if mdl is not a recursive, backward looking model.
  # This implementation has not been tested yet for models with feedback or forward
  # looking methods.

  endo_names <- mdl$get_endo_names()

  # Check if final_destinations are endogenous variables
  no_endo_names <- setdiff(final_destinations$var, endo_names)
  if (length(no_endo_names) > 0) {
    stop("The following variables are not endogenous model variables:\n",
         paste(no_endo_names, collapse = ", "), ".")
  }

  # TODO: check that final sources are endogenous

  # Column period of vars or derivable may contain period as integer
  # (a year).
  periods <- as.character(final_destinations$period)
  vertices_final_dest <- paste0(final_destinations$var, "[", periods, "]")

  has_final_sources <- !is.null(final_sources)
  if (has_final_sources) {
    periods <- as.character(final_sources$period)
    vertices_final_src <- paste0(final_sources$var, "[", periods, "]")
  }

  has_solvable <- !is.null(solvable)
  if (has_solvable) {
    # TODO: give an error if solvable has overlap with observations.
    # Also check that they are endogenous variables.
    # TODO: also give an error in solvable has overlap with final_sources.
    periods <- as.character(solvable$period)
    vertices_solvable <- paste0(solvable$var, "[", periods, "]")
  } else {
    vertices_solvable <- character(0)
  }

  # TODO: check that there is there is no overlap between final_destinations
  # and solvable.

  min_period_obs <- min(period(observed_data$period))

  periods <- observed_data$period
  vertices_observed <- paste0(observed_data$var, "[", periods, "]")

  problem_vertices <- intersect(vertices_solvable, vertices_observed)
  if (length(problem_vertices)) {
    stop("The following solvable vertices are actually observed",
         paste(problem_vertices, collapse = ", "))
  }


  # Convert dependency structure to a list (working with a list is more efficient)
  dep_struc <- split(dep_struc, dep_struc$lhs) |>
    lapply(FUN = \(x) {
      rename(x, var = "rhs", period = "lag") |>
        select(-c("lhs"))
    }
    )

  g <- make_empty_graph()

  # Function to create a new vertex.
  add_vertex <- function(g, vertex_name, observed, is_endo, is_final_dest) {
    g <- add_vertices(g, 1, name = vertex_name, ok = NA,
                      observed = observed, calculable = NA,
                      is_endo = is_endo,
                      is_final_dest = is_final_dest,
                      msg = NA)
    if (has_solvable) {
      idx <- which(igraph::V(g)$name == vertex_name)
      g <- igraph::set_vertex_attr(g, "solvable", index = idx,
                                   value =  vertex_name %in% vertices_solvable)
    }
    g
  }

  .find_deps <- function(var_per) {

    # TODO: hoe voorkom je oneindige recursie, bijv. voor simultane
    # modellen.?
    # waarschijnlijk als het year kleiner dan een drempel is?
    # Of als het jaar kleiner is dan het minale jaar van de data?

    vertex_name <- as.character(var_per)
    var <- var_per$var
    per <- var_per$period

    is_endo <- var %in% endo_names

    # Check if the variable is observed at the specified period
    observed <- vertex_name %in% vertices_observed
    is_final_dest <- vertex_name %in% vertices_final_dest
    is_solvable <- vertex_name %in%  vertices_solvable

    color_ok <- if (is_final_dest) "cyan" else "green"
    color_not_ok <- if (is_final_dest) "purple" else "red"

    if (vertex_name %in% V(g)$name) {
      ok <- V(g)[name == vertex_name]$ok
      # if OK is not NA, the vertex creating has not finished yet.
      if (!is.na(ok)) {
        return(ok)
      }
    } else {
      # Create a new vertex.
      g <<- add_vertex(g, vertex_name = vertex_name, observed = observed,
                       is_endo = is_endo, is_final_dest = is_final_dest)
    }

    name <- NULL # prevent messages

    # Compute the index the vertex with name 'vertex_name'
    idx <- which(igraph::V(g)$name == vertex_name)

    # Check if vertex is a final source, in that case return.
    if (has_final_sources) {
      is_final_src <- vertex_name %in% vertices_final_src
      g <<- igraph::set_vertex_attr(g, "is_final_src", index = idx, value = is_final_src)
      if (is_final_src) {
        g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = TRUE)
        g <<- igraph::set_vertex_attr(g, "color", index = idx, value = "yellow")
        return(TRUE)
      }
    }

    # Check if the variable is in the data
    if (observed && (!is_final_dest || stop_if_final_dest_observed)) {
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = TRUE)
      g <<- igraph::set_vertex_attr(
        g, "color", index = idx,
        value = if (is_final_dest) "cyan" else "lightgrey"
      )
      return(TRUE)
    }

    # Check if the variable is exogenous. If not in data, this
    # is an error
    if (!is_endo) {
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = FALSE)
      g <<- igraph::set_vertex_attr(g, "msg", index = idx, value = "Missing exo")
      g <<- igraph::set_vertex_attr(g, "color", index = idx, value = color_not_ok)
      return(FALSE)
    }

    # Check if the period outside the period range
    if (per < min_period_obs - 1) {
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = FALSE)
      g <<- igraph::set_vertex_attr(g, "calculable", index = idx, value = FALSE)
      g <<- igraph::set_vertex_attr(g, "color", index = idx, value = color_not_ok)
      g <<- igraph::set_vertex_attr(g, "msg", index = idx, value = "outside period range")
      return(FALSE)
    }

    # Now check dependencies

    deps_new <- dep_struc[[var]]

    if (is.null(deps_new)) {
      # Als de variabele geen dependencies heeft en geen exogene variabele is,
      # heeft de variabele de structuur zoals x = 0 (aan de rechterkant alleen
      # getallen of parameters).
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = TRUE)
      g <<- igraph::set_vertex_attr(g, "calculable", index = idx, value = TRUE)
      g <<- igraph::set_vertex_attr(g, "color", index = idx, value = color_ok)
      return(TRUE)
    }

    deps_new <- mutate(deps_new, period = as.character(.data$period + per))

    if (ignore_observed) {
      # Remove observations from the dependencies
      deps_new <- anti_join(deps_new, observed_data,  by = c("var", "period"))
      if (nrow(deps_new) == 0) {
        # The variable only depends on observed variables.
        g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = TRUE)
        g <<- igraph::set_vertex_attr(g, "calculable", index = idx, value = TRUE)
        g <<- igraph::set_vertex_attr(g, "color", index = idx, value = color_ok)
        return(TRUE)
      }
    }

    # convert dependencies to a list of dependencies
    deps_new <- split(deps_new, seq_len(nrow(deps_new)))
    deps_new <- lapply(deps_new, FUN = \(x) do.call(new_var_period, x))

    for (dep_new in deps_new) {
      vertex_name_dep <- as.character(dep_new)
      if (!vertex_name_dep %in% V(g)$name) {
        var_name <- sub("\\[.+", "", vertex_name_dep)
        g <<- add_vertex(
          g, vertex_name = vertex_name_dep,
          observed = vertex_name_dep %in% vertices_observed,
          is_endo =  var_name %in% endo_names,
          is_final_dest =  vertex_name_dep %in% vertices_final_dest
        )
      }
      g <<- add_edges(g, c(vertex_name_dep, vertex_name))
    }

    # recursively find dependencies
    ok <- sapply(deps_new, FUN = .find_deps)
    stopifnot(!any(is.na(ok)))

    calculable <- all(ok)

    ok <- calculable || observed

    # If this node is not calculable, but only depends on vertices that are
    # either calculable or observed or in solvable_vertices,
    # then ignore this one.
    if (!ok && has_solvable) {
      children <- neighbors(g, vertex_name, mode = "in")
      ok <- all(V(g)[children]$ok |
          V(g)[children]$name %in% vertices_solvable
      )
    }

    # Updaten dependency structure
    if (ok) {
      # Calculable or observed
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = TRUE)
      g <<- igraph::set_vertex_attr(g, "calculable", index = idx, value = calculable)
      color <- if (is_solvable)  "pink" else color_ok
      g <<- igraph::set_vertex_attr(g, "color", index = idx, value = color)
      return(TRUE)
    } else {
      # Not calculable or observed.
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = FALSE)
      g <<- igraph::set_vertex_attr(g, "calculable", index = idx, value = FALSE)
      color <- if (is_solvable) "pink" else color_not_ok
      g <<- igraph::set_vertex_attr(g, "color", index = idx, value = color)
      # Remove incoming edges from an incalculable node, except if this is a
      # final node.
      if (!is_final_dest) {
        g <<- delete_edges(g, incident(g, vertex_name, mode = "in"))
      }
      return(FALSE)
    }
  }

  for (i in seq_len(nrow(final_destinations))) {
    var_per <- new_var_period(final_destinations[i, "var"],
                              final_destinations[i, "period"])
    .find_deps(var_per)
  }

  # TODO: give the vertices colour at this point

  # Find all vertices reachable from target vertices (including themselves)
  reachable <- unique(unlist(lapply(vertices_final_dest, function(v) {
    subcomponent(g, v, mode = "all")
  })))

  # Keep only the reachable vertices
  g <- induced_subgraph(g, reachable)

  # Remove isolated vertices (degree = 0) that are not final vertices.
  isolated <- which(degree(g) == 0 & !V(g)$name %in% vertices_final_dest)
  g <- delete_vertices(g, isolated)

  # Remove solvable vertices without a path to final destinations.
  if (has_solvable) {

    is_final_dest <- NULL # prevent error

    # Get vertices that are final destinations
    final_dest_vertices <- V(g)[is_final_dest == TRUE]

    # Get all vertices that can reach any final destination
    # Use mode="out" to follow outgoing edges
    reachable_from_any <- unique(unlist(lapply(final_dest_vertices, function(v) {
      subcomponent(g, v, mode = "in")
    })))

    # Get solvable vertices
    solvable_vertices <- V(g)[solvable == TRUE]

    # Find solvable vertices that are NOT in the reachable set
    to_remove <- setdiff(solvable_vertices, reachable_from_any)

    # Remove these vertices
    g <- delete_vertices(g, to_remove)

    # TODO: print deleted vertices, these variables cannot be solved
    # (but are bycatch).
    # TODO: remove calculated nodes.
  }

  return(g)
}

as_data_frame_deps <- function(g) {
  igraph::as_data_frame(g, what = "vertices") |>
    remove_rownames() |>
    #  select("name", "type", "is_final", "color") |>
    separate("name", into = c("var", "period"), sep = "\\[|\\]",
             extra = "drop")
}
