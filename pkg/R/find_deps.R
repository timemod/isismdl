#' Recursively find all dependencies of one or more target variables.
#'
#' Walks backwards through the model's equation structure starting from each
#' variable-period pair in `final_destinations`, building a directed graph
#' where an edge from A to B means "B depends on A". Each vertex is annotated
#' with `ok`, `calculable`, `observed`, `color` and `msg`. Recursion stops
#' when a vertex is observed, is a final source, is a missing exogenous
#' variable, or its period falls below the minimum observed period. Cycles
#' raise an error immediately. After recursion the graph is pruned to vertices
#' with a path to a final destination.
#'
#' @param final_destinations Data frame with columns `var` and `period`.
#' @param final_sources Optional data frame with `var` and `period`;
#'   recursion stops at these vertices.
#' @param solvable Optional data frame with `var` and `period` of
#'   variables to be solved numerically rather than computed forward.
#' @param observed_data Data frame with `var` and `period` of observed values.
#' @param dep_struc Dependency structure from `get_dep_struct(one_lag_per_row = TRUE)`.
#' @param mdl The `IsisMdl` object (must be recursive, no feedback, no leads).
#' @param ignore_observed Logical; if `TRUE`, observed deps are stripped before recursing.
#' @param stop_if_final_dest_observed Logical; if `TRUE` (default), stops when
#'   a final destination is already observed.
#' @return A directed igraph object (edges point from dependency to dependent).
#'
#' @importFrom igraph add_edges make_empty_graph neighbors induced_subgraph
#'   delete_edges incident subcomponent delete_vertices
#'   add_vertices V degree set_vertex_attr
#' @importFrom dplyr rename mutate anti_join
#' @importFrom rlang .data
#'
#' @noRd
find_deps <- function(final_destinations, final_sources = NULL,
                      solvable = NULL, observed_data,
                      dep_struc, mdl, ignore_observed = FALSE,
                      stop_if_final_dest_observed = TRUE) {

  # Color constants for vertices
  COLOR_FINAL_SRC      <- "yellow"
  COLOR_SOLVABLE       <- "pink"
  COLOR_FINAL_DEST_OK  <- "cyan"
  COLOR_OBSERVED       <- "lightgrey"
  COLOR_OK             <- "green"
  COLOR_FINAL_DEST_ERR <- "purple"
  COLOR_ERR            <- "red"

  if (mdl$get_maxlead() > 0) {
    stop("find_deps does not support models with leads.")
  }
  if (length(mdl$get_simul_names()) > 0) {
    stop("find_deps only supports recursive models.")
  }

  if (is.null(observed_data)) {
    observed_data <- mdl$get_observed_data()
  }

  endo_names <- mdl$get_endo_names()

  # Helper function to check if variables are endogenous
  check_endo <- function(df, name) {
    if (is.null(df) || nrow(df) == 0) return()
    no_endo <- setdiff(df$var, endo_names)
    if (length(no_endo) > 0) {
      stop(sprintf("The following %s variables are not endogenous model variables:\n%s.",
                   name, paste(no_endo, collapse = ", ")))
    }
  }

  # Check if variables are endogenous
  check_endo(final_destinations, "final destination")
  check_endo(final_sources, "final source")
  check_endo(solvable, "solvable")

  # Column period of vars or derivable may contain period as integer
  # (a year).
  periods <- as.character(final_destinations$period)
  vertices_final_dest <- paste0(final_destinations$var, "[", periods, "]")

  has_final_sources <- !is.null(final_sources)
  if (has_final_sources) {
    periods <- as.character(final_sources$period)
    vertices_final_src <- paste0(final_sources$var, "[", periods, "]")
  } else {
    vertices_final_src <- character(0)
  }

  has_solvable <- !is.null(solvable)
  if (has_solvable) {
    periods <- as.character(solvable$period)
    vertices_solvable <- paste0(solvable$var, "[", periods, "]")
  } else {
    vertices_solvable <- character(0)
  }

  if (nrow(observed_data) > 0) {
    min_period_obs <- min(period(observed_data$period))
  } else {
    min_period_obs <- start_period(mdl$get_data_period())
  }

  periods <- observed_data$period
  vertices_observed <- paste0(observed_data$var, "[", periods, "]")

  # Helper function to check for overlaps and list ALL offending variables
  check_overlap <- function(v1, v2, msg) {
    problem_vertices <- intersect(v1, v2)
    if (length(problem_vertices) > 0) {
      stop(sprintf("%s:\n%s", msg, paste(problem_vertices, collapse = ", ")))
    }
  }

  check_overlap(vertices_solvable, vertices_observed,
                "The following solvable vertices are actually observed")
  check_overlap(vertices_solvable, vertices_final_src,
                "The following solvable vertices are also final sources")
  check_overlap(vertices_final_dest, vertices_solvable,
                "The following final destination vertices are also solvable")


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
                      is_final_src = vertex_name %in% vertices_final_src,
                      msg = NA)
    if (has_solvable) {
      idx <- which(igraph::V(g)$name == vertex_name)
      g <- igraph::set_vertex_attr(g, "solvable", index = idx,
                                   value =  vertex_name %in% vertices_solvable)
    }
    g
  }

  .find_deps <- function(var_per) {

    # Recursion is stopped when the period is before the threshold (min_period_obs).

    vertex_name <- as.character(var_per)
    var <- var_per$var
    per <- var_per$period

    is_endo <- var %in% endo_names

    # Check if the variable is observed at the specified period
    observed <- vertex_name %in% vertices_observed
    is_final_dest <- vertex_name %in% vertices_final_dest
    is_solvable <- vertex_name %in% vertices_solvable

    color_ok <- if (is_final_dest) COLOR_FINAL_DEST_OK else COLOR_OK
    color_not_ok <- if (is_final_dest) COLOR_FINAL_DEST_ERR else COLOR_ERR

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
        g <<- igraph::set_vertex_attr(g, "color", index = idx, value = COLOR_FINAL_SRC)
        return(TRUE)
      }
    }

    # Check if the variable is in the data
    if (observed && (!is_final_dest || stop_if_final_dest_observed)) {
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = TRUE)
      g <<- igraph::set_vertex_attr(
        g, "color", index = idx,
        value = if (is_final_dest) COLOR_FINAL_DEST_OK else COLOR_OBSERVED
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
      color <- if (is_solvable) COLOR_SOLVABLE else color_ok
      g <<- igraph::set_vertex_attr(g, "color", index = idx, value = color)
      return(TRUE)
    } else {
      # Not calculable or observed.
      g <<- igraph::set_vertex_attr(g, "ok", index = idx, value = FALSE)
      g <<- igraph::set_vertex_attr(g, "calculable", index = idx, value = FALSE)
      color <- if (is_solvable) COLOR_SOLVABLE else color_not_ok
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
    var_per <- new_var_period(final_destinations$var[[i]],
                              final_destinations$period[[i]])
    .find_deps(var_per)
  }

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
    reachable_from_any <- unique(unlist(lapply(final_dest_vertices, function(v) {
      subcomponent(g, v, mode = "in")
    })))

    # Get solvable vertices
    solvable_vertices <- V(g)[solvable == TRUE]

    all_vertices <- V(g)

    # Find solvable vertices that are NOT in the reachable set
    to_remove <- setdiff(all_vertices, reachable_from_any)

    removed_vertex_names <- names(V(g))[to_remove]

    removed_solvable <- intersect(removed_vertex_names, names(solvable_vertices))
    if (length(removed_solvable)) {
      message("The following solvable variables have been removed because ",
              "there is not a calculable path to observations:\n",
              paste(removed_solvable, collapse = ", "))
    }

    # Remove these vertices
    g <- delete_vertices(g, to_remove)

    # Only keep the vertices that can be reached from solvable_vertices.
    solvable_vertices <- V(g)[solvable == TRUE]
    can_reach_solvable <- unique(unlist(lapply(solvable_vertices, function(v) {
      subcomponent(g, v, mode = "out")
    })))
    g <- induced_subgraph(g, can_reach_solvable)
  }

  return(g)
}
