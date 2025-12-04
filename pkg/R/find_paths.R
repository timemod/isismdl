#' @importFrom dplyr mutate select
#' @importFrom igraph graph_from_data_frame all_simple_paths
#' @importFrom graphics plot

find_paths <- function(derived_var, observable_var, dependency_structure) {

  # Create graph of model
  struc <- dependency_structure |>
    mutate(rhs = ifelse(.data$lag == 0, .data$rhs,
                        paste0(.data$rhs, "(", .data$lag, ")"))) |>
    select("rhs", "lhs")
  g <- graph_from_data_frame(struc[, c("rhs", "lhs")], directed = TRUE)
  plot(g)

  convert_path <- function(path, g) {
    path <- names(V(g))[path]
    lag_pattern <- "^([a-zA_Z]+)(\\((-\\d+)\\))?$"
    ret <- str_match(path, lag_pattern)
    vars <- ret[, 2]
    lags <- as.numeric(ret[, 4])
    lags <- ifelse(is.na(lags), 0, lags)
    data.frame(var = vars, lag = lags)
  }

  # TODO: of moeten we juist het kortste pad hebben?
  paths <- all_simple_paths(g, from = derived_var, to = observable_var)
  lapply(paths, convert_path, g = g)
}
