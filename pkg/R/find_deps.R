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
#' @param dep_struc The model's dependency structure. This can be either a
#'   data frame (retrieved via `mdl$get_dep_struct(one_lag_per_row = TRUE)`) or
#'   a pre-processed list (retrieved via `dep_struct_to_list()`).
#' @param mdl The `IsisMdl` object (must be recursive, no feedback, no leads).
#' @param ignore_observed Logical; if `TRUE`, observed deps are stripped before recursing.
#' @param stop_if_final_dest_observed Logical; if `TRUE` (default), stops when
#'   a final destination is already observed.
#' @return A directed igraph object (edges point from dependency to dependent).
#'
#' @importFrom igraph add_edges make_empty_graph neighbors induced_subgraph
#' @importFrom igraph delete_edges incident subcomponent delete_vertices
#' @importFrom igraph add_vertices V degree set_vertex_attr
#' @importFrom dplyr rename mutate anti_join
#' @importFrom rlang .data
#'
#' @noRd

find_deps_internal <- function(final_destinations, final_sources = NULL,
                               solvable = NULL, observed_data, mdl,
                               dep_struc = mdl$get_dep_struct(one_lag_per_row = TRUE),
                               ignore_observed = FALSE,
                               stop_if_final_dest_observed = TRUE) {

  # Color constants for vertices
  COLOR_FINAL_SRC      <- "yellow"
  COLOR_SOLVABLE       <- "pink"
  COLOR_FINAL_DEST_OK  <- "cyan"
  COLOR_OBSERVED       <- "lightgrey"
  COLOR_CALCULABLE     <- "green"
  COLOR_FINAL_DEST_ERR <- "purple"
  COLOR_ERR            <- "red"

  endo_names <- mdl$get_endo_names()

  vertices_final_dest <- to_vertex_names(final_destinations)

  has_final_sources <- !is.null(final_sources)
  vertices_final_src <- if (has_final_sources) {
    to_vertex_names(final_sources)
  } else {
    character(0)
  }

  has_solvable <- !is.null(solvable)
  vertices_solvable <- if (has_solvable) {
    to_vertex_names(solvable)
  } else {
    character(0)
  }

  # Determine the minimum period with data.
  # Recursion stops when the period smaller than this period minus 1.
  data_period <- mdl$get_data_period()
  if (is.null(data_period)) {
    stop("The model period has not been set. Set the model period with ",
         "set_period() or init_data().")
  }
  min_period_data <- start_period(data_period)
  if (nrow(observed_data) > 0) {
    min_period_data <- min(min_period_data, min(period(observed_data$period)))
  }

  vertices_observed <- to_vertex_names(observed_data)

  g <- make_empty_graph()

  # Function to create a new vertex.
  add_vertex <- function(g, vertex_name, observed, is_endo, is_final_dest,
                         is_solvable) {

    g <- add_vertices(g, 1, name = vertex_name, ok = NA,
                      observed = observed, calculable = NA,
                      is_endo = is_endo,
                      is_final_dest = is_final_dest,
                      solvable = if (has_solvable) is_solvable else NULL,
                      msg = NA)
    g
  }

  .find_deps <- function(var_per) {

    # Recursion is stopped when the period is smaller than min_period_data - 1

    vertex_name <- as.character(var_per)
    vertex_exists <- vertex_name %in% V(g)$name

    # If the vertex already exists in the graph, then check if the recursion
    # for this vertex has already been completed. This is the case when
    # attribute 'ok' is not NA. In that case just return the status (o.k.)
    # of this vertex.
    if (vertex_exists) {
      # Compute the index of the vertex with name 'vertex_name':
      idx <- which(V(g)$name == vertex_name)
      ok <- V(g)[idx]$ok
      if (!is.na(ok)) {
        return(ok)
      }
    }

    var <- var_per$var
    per <- var_per$period

    is_endo <- var %in% endo_names
    observed <- vertex_name %in% vertices_observed
    is_final_dest <- vertex_name %in% vertices_final_dest
    is_solvable <- vertex_name %in% vertices_solvable

    # Create a new vertex.
    if (!vertex_exists) {
      g <<- add_vertex(g, vertex_name = vertex_name, observed = observed,
                       is_endo = is_endo, is_final_dest = is_final_dest,
                       is_solvable = is_solvable)
      # Compute the index of the vertex with name 'vertex_name':
      idx <- which(V(g)$name == vertex_name)
    }

    # Color for vertex with an error.
    color_not_ok <- if (is_final_dest) COLOR_FINAL_DEST_ERR else COLOR_ERR

    # Check if vertex is a final source, in that case return.
    if (has_final_sources) {
      is_final_src <- vertex_name %in% vertices_final_src
      g <<- set_vertex_attr(g, "is_final_src", index = idx, value = is_final_src)
      if (is_final_src) {
        g <<- set_vertex_attr(g, "ok", index = idx, value = TRUE)
        g <<- set_vertex_attr(g, "color", index = idx, value = COLOR_FINAL_SRC)
        return(TRUE)
      }
    }

    # Check if the variable is observed.
    if (observed && (!is_final_dest || stop_if_final_dest_observed)) {
      g <<- set_vertex_attr(g, "ok", index = idx, value = TRUE)
      g <<- set_vertex_attr(
        g, "color", index = idx,
        value = if (is_final_dest) COLOR_FINAL_DEST_OK else COLOR_OBSERVED
      )
      return(TRUE)
    }

    # Check if the variable is exogenous. This is an error, because the variable
    # is not observed.
    if (!is_endo) {
      g <<- set_vertex_attr(g, "ok", index = idx, value = FALSE)
      g <<- set_vertex_attr(g, "msg", index = idx, value = "Missing exo")
      g <<- set_vertex_attr(g, "color", index = idx, value = color_not_ok)
      return(FALSE)
    }

    # Break the recursion if the period is before min_period_data - 1.
    if (per < min_period_data - 1) {
      g <<- set_vertex_attr(g, "ok", index = idx, value = FALSE)
      g <<- set_vertex_attr(g, "color", index = idx, value = color_not_ok)
      g <<- set_vertex_attr(g, "msg", index = idx, value = "outside period range")
      return(FALSE)
    }

    # Color for a vertex for an endogenous variable that is OK:
    color_endo_ok <- if (is_final_dest) {
      COLOR_FINAL_DEST_OK
    } else if (is_solvable) {
      COLOR_SOLVABLE
    } else {
      COLOR_CALCULABLE
    }

    # Now check dependencies

    deps_new <- dep_struc[[var]]

    if (is.null(deps_new)) {
      # The variable is endogenous but has no dependencies.
      # This is the case for equations such as x = 0 (the right hand side
      # of the equation only contains constants or parameters).
      g <<- set_vertex_attr(g, "ok", index = idx, value = TRUE)
      g <<- set_vertex_attr(g, "calculable", index = idx, value = TRUE)
      g <<- set_vertex_attr(g, "color", index = idx, value = color_endo_ok)
      return(TRUE)
    }

    deps_new <- mutate(deps_new, period = as.character(.data$period + per))

    if (ignore_observed) {
      # Remove observations from the dependencies
      deps_new <- anti_join(deps_new, observed_data,  by = c("var", "period"))
      if (nrow(deps_new) == 0) {
        # The variable only depends on observed variables.
        g <<- set_vertex_attr(g, "ok", index = idx, value = TRUE)
        g <<- set_vertex_attr(g, "calculable", index = idx, value = TRUE)
        g <<- set_vertex_attr(g, "color", index = idx, value = color_endo_ok)
        return(TRUE)
      }
    }

    # convert dependencies to a list of dependencies
    deps_new <- split(deps_new, seq_len(nrow(deps_new)))
    deps_new <- lapply(deps_new, FUN = \(x) do.call(new_var_period, x))

    # Recursively find dependencies
    calculable <- TRUE
    for (dep_new in deps_new) {
      vertex_ok <- .find_deps(dep_new)
      vertex_name_dep <- as.character(dep_new)
      if (vertex_ok || is_final_dest) {
        g <<- add_edges(g, c(vertex_name_dep, vertex_name))
      }
      if (!vertex_ok && calculable) {
        calculable <- FALSE
        # If this node not a final destination, the dependency check for
        # other nodes may be skipped.
        if (!is_final_dest) {
          # Remove incoming edges from an incalculable node if this is not a
          # final
          g <<- delete_edges(g, incident(g, vertex_name, mode = "in"))
          break
        }
      }
    }

    # A vertex for an endogenous variable is OK if it is calculable, observed or
    # solvable.
    ok <- calculable || observed || is_solvable

    # Set vertex attributes ok, calculable and color:
    g <<- set_vertex_attr(g, "ok", index = idx, value = ok)
    g <<- set_vertex_attr(g, "calculable", index = idx, value = calculable)
    g <<- set_vertex_attr(g, "color", index = idx,
                          value = if (ok) color_endo_ok else color_not_ok)

    return(ok)
  }

  for (i in seq_len(nrow(final_destinations))) {
    var_per <- new_var_period(final_destinations$var[[i]],
                              final_destinations$period[[i]])
    .find_deps(var_per)
  }

  # --- PRUNING PHASE ---
  # The recursive search identifies all potential dependencies, but the resulting
  # graph may contain vertices that are irrelevant to the specific calculation
  # goal. Pruning is performed in two primary steps to refine the graph:

  # Step 1: Remove vertices that have no path to a final destination.
  # These are vertices that have a broken path to a final destination because an
  # intermediate vertex is incalculable.
  # We identify all vertices that have a path *to* a final destination by
  # traversing the graph backwards from the destinations.
  final_dest_vertices <- V(g)[V(g)$is_final_dest == TRUE]

  vertices_with_path_to_final_dest <-
    unique(
      unlist(
        lapply(final_dest_vertices,
               function(v) subcomponent(g, v, mode = "in"))
      )
    )
  g <- induced_subgraph(g, vertices_with_path_to_final_dest)

  # Step 2: Keep only vertices that are reachable from the specified "solvable"
  # variables. This isolates the portion of the model that relates to these
  # variables.
  if (has_solvable) {
    # Only consider solvable vertices that are actually in the graph
    vertices_solvable_in_g <- intersect(vertices_solvable, names(V(g)))
    vertices_with_path_from_solvable <-
      unique(
        unlist(
          lapply(vertices_solvable_in_g,
            function(v) subcomponent(g, v, mode = "out")
          )
        )
      )
    g <- induced_subgraph(g, vertices_with_path_from_solvable)
  }

  # --- NOTIFICATIONS ---

  if (has_solvable) {
    # Issue a message if any solvable variables are missing from the final graph
    removed_solvable <- setdiff(vertices_solvable, names(V(g)))
    if (length(removed_solvable) > 0) {
      rem_solv_df <- vertex_names_to_df(removed_solvable)
      message("The following solvable variables have been removed because\n",
              "they have no calculable path to a final destination:\n",
              paste(capture.output(print(as.data.frame(rem_solv_df), row.names = FALSE)),
                    collapse = "\n"))
    }
  }

  # Check for solvables that are actually calculable
  if (has_solvable) {
    v_solvable_calculable <- V(g)[V(g)$solvable & V(g)$calculable]
    if (length(v_solvable_calculable) > 0) {
      solv_calc_df <- as_data_frame_deps(induced_subgraph(g, v_solvable_calculable)) |>
        select("var", "period")
      message("The following solvables are calculable, no need to solve them:\n",
              paste(capture.output(print(as.data.frame(solv_calc_df), row.names = FALSE)),
                    collapse = "\n"))
    }
  }

  return(g)
}

# This function is being called as method on an object of class IsisMdl
find_deps <- function(mdl, final_destinations, final_sources,
                      solvable, observed_data, ignore_observed,
                      stop_if_final_dest_observed, dep_struc) {

  # /start/ TEMPORARY SOLUTION: Set default values for optional arguments
  # TODO: some jobs still use isismdl:::find_deps,
  # this can be removed once all jobs are updated and is being used as method
  if (missing(final_sources)) final_sources <- NULL
  if (missing(solvable)) solvable <- NULL
  if (missing(observed_data)) observed_data <- NULL
  if (missing(ignore_observed)) ignore_observed <- FALSE
  if (missing(stop_if_final_dest_observed)) stop_if_final_dest_observed <- TRUE
  # /end/ TEMPORARY SOLUTION

  if (missing(dep_struc) || is.null(dep_struc)) {
    dep_struc <- mdl$get_dep_struct(one_lag_per_row = TRUE)
  }
  dep_struc <- dep_struct_to_list(dep_struc)

  if (mdl$get_maxlead() > 0) {
    stop("find_deps does not support models with leads.")
  }
  if (length(mdl$get_simul_names()) > 0) {
    stop("find_deps only supports recursive models.")
  }

  if (is.null(observed_data)) {
    observed_data <- get_observed_data(mdl$get_data())
  }
  check_var_period_df(final_destinations, "final destinations")
  check_var_period_df(final_sources, "final sources")
  check_var_period_df(solvable, "solvable")
  check_var_period_df(observed_data, "observed data", check_value = TRUE)

  endo_names <- mdl$get_endo_names()

  # Check if variables are endogenous
  check_endo(final_destinations$var, endo_names, "final destination")
  check_endo(final_sources$var, endo_names, "final source")
  check_endo(solvable$var, endo_names, "solvable")

  # Check for overlaps
  v_dest <- to_vertex_names(final_destinations)
  v_src  <- to_vertex_names(final_sources)
  v_solv <- to_vertex_names(solvable)
  v_obs  <- to_vertex_names(observed_data)

  check_overlap(v_solv, v_obs,
                "The following solvable vertices are actually observed")
  check_overlap(v_solv, v_src,
                "The following solvable vertices are also final sources")
  check_overlap(v_dest, v_solv,
                "The following final destination vertices are also solvable")


  find_deps_internal(final_destinations = final_destinations,
                     final_sources = final_sources,
                     solvable = solvable,
                     observed_data = observed_data,
                     dep_struc = dep_struc,
                     mdl = mdl, ignore_observed = ignore_observed,
                     stop_if_final_dest_observed = stop_if_final_dest_observed)
}
