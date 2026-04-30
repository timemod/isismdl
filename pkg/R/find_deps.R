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
#'   delete_edges incident subcomponent delete_vertices
#'   add_vertices V degree set_vertex_attr
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

  if (nrow(observed_data) > 0) {
    min_period_obs <- min(period(observed_data$period))
  } else {
    min_period_obs <- start_period(mdl$get_data_period())
  }

  vertices_observed <- to_vertex_names(observed_data)

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

    # Recursion is stopped when the period is before the threshold (min_period_obs).

    vertex_name <- as.character(var_per)
    var <- var_per$var
    per <- var_per$period

    is_endo <- var %in% endo_names

    # Check if the variable is observed at the specified period
    observed <- vertex_name %in% vertices_observed
    is_final_dest <- vertex_name %in% vertices_final_dest
    is_solvable <- vertex_name %in% vertices_solvable

    color_ok <- if (is_final_dest) COLOR_FINAL_DEST_OK else COLOR_CALCULABLE
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

    # Check if the variable is observed.
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
    calculable <- TRUE
    for (dep_new in deps_new) {
      vertex_ok <- .find_deps(dep_new)

      # If the dependency is not calculable, but it is solvable, we can still
      # consider it "calculable" for the purpose of the current node.
      if (!vertex_ok && has_solvable) {
        vertex_name_dep <- as.character(dep_new)
        if (vertex_name_dep %in% vertices_solvable) {
          vertex_ok <- TRUE
        }
      }

      if (!vertex_ok) {
        calculable <- FALSE
        break
      }
    }

    ok <- calculable || observed

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

  # --- PRUNING PHASE ---
  # After the recursive search, the graph may contain variables that are not
  # strictly required for the final destinations. This happens when:
  # 1. An intermediate variable cannot be calculated (e.g., missing data), breaking the path.
  # 2. Solvable variables are provided, in which case we only care about the
  #    downstream path from the solvables to the final destinations.

  # Step 0 >1: naam veranderen
  # Alleen vertices houden die een pad vanaf solvable.
  if (has_solvable) {
    can_reach_solvable <- unique(unlist(lapply(vertices_solvable, function(v) {
      subcomponent(g, v, mode = "out")
    })))
    g <- induced_subgraph(g, can_reach_solvable)
  }

  # Step 1: Remove solvables that do not have a path to any final destination.

  # Find all vertices that can reach a final destination by traversing backwards (mode = "in").
  final_dest_vertices <- V(g)[V(g)$is_final_dest == TRUE]

  vertices_with_path_to_final_dest <-
    unique(
      unlist(
        lapply(final_dest_vertices,
               function(v) subcomponent(g, v, mode = "in"))
      )
    )

  # Find vertices that are NOT in the reachable set
  vertices_to_remove <- setdiff(V(g), vertices_with_path_to_final_dest)
  removed_vertex_names <- names(V(g))[vertices_to_remove]

  # Step 2: Keep only the path from solvable variables to final destinations.
  # By traversing forwards (mode = "out") from the solvables, we keep the
  # solvable variables and their dependents. This effectively removes:
  # - Final destinations that do not depend on any solvable.
  # - Upstream dependencies of the solvable variables (since they are treated
  #   as starting points for the numerical solver).
  if (has_solvable) {
    # Issue a message if any solvable variables are among those being removed
    removed_solvable <- intersect(removed_vertex_names, vertices_solvable)
    if (length(removed_solvable)) {
      rem_solv_df <- as_data_frame_deps(induced_subgraph(g, removed_solvable))
      rem_solv_df <- as.data.frame(rem_solv_df[, c("var", "period")])
      message("The following solvable variables have been removed because ",
              "they have no calculable path to a final destination:\n",
              paste(capture.output(print(rem_solv_df, row.names = FALSE)), collapse = "\n"))
    }
  }
  # Delete vertices that cannot reach a final destination
  g <- delete_vertices(g, vertices_to_remove)

  # Check for solvables that are actually calculable
  if (has_solvable) {
    v_solvable_calculable <- igraph::V(g)[igraph::V(g)$solvable & igraph::V(g)$calculable]
    if (length(v_solvable_calculable) > 0) {
      solv_calc_df <- as_data_frame_deps(induced_subgraph(g, v_solvable_calculable)) |>
        select("var", "period")
      message("The following solvables are calculable, no need to solve them:\n",
              paste(capture.output(print(solv_calc_df, row.names = FALSE)), collapse = "\n"))
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

  # Check for duplicates
  check_unique(v_dest, "final destination")
  check_unique(v_src, "final source")
  check_unique(v_solv, "solvable")
  check_unique(v_obs, "observed data")

  find_deps_internal(final_destinations = final_destinations,
                     final_sources = final_sources,
                     solvable = solvable,
                     observed_data = observed_data,
                     dep_struc = dep_struc,
                     mdl = mdl, ignore_observed = ignore_observed,
                     stop_if_final_dest_observed = stop_if_final_dest_observed)
}

