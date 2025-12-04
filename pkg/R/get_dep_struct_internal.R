# internal function to create the dependency structure from the model_text
#' @importFrom utils read.csv
get_dep_struct_internal <- function(model_text, active_endo_names, one_lag_per_row = FALSE) {

  mdl_file_tmp1 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  mdl_file_tmp2 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  dep_file_tmp  <- tempfile(pattern = "isismdl_", fileext = ".dep")

  writeLines(model_text, mdl_file_tmp1)

  # substitute user functions, the dependency structure cannot be determined for
  # models with user functions

  ok <- .Call(C_convert_mdl_file_c, mdl_file_tmp1, mdl_file_tmp2, NULL,
              NULL, list(substitute = TRUE))
  stopifnot(ok)

  ok <- .Call(C_gen_dep_file, mdl_file_tmp2, dep_file_tmp)
  stopifnot(ok)

  dep_data <- read.csv(dep_file_tmp, header = FALSE,
                       col.names = c("lhs", "rhs", "lags"),
                       stringsAsFactors = FALSE)

  # remove dependency of inactive equations
  row_sel <- dep_data$lhs %in% active_endo_names
  dep_data <- dep_data[row_sel, , drop = FALSE]

  # sort lhs names alphabetically
  dep_data <- dep_data[order(dep_data$lhs), , drop = FALSE]
  rownames(dep_data) <- NULL

  file.remove(c(mdl_file_tmp1, mdl_file_tmp2, dep_file_tmp))

  if (one_lag_per_row) {
    # clean quotes and split
    lags_clean <- gsub('"', "", dep_data$lags)
    lags_list  <- strsplit(lags_clean, "\\s+")
    # handle possible empty strings safely
    lens <- vapply(lags_list,
      function(x) if (length(x) == 1 && x == "") 0L else length(x),
      integer(1)
    )
    # replicate lhs/rhs according to counts
    lhs_rep <- rep(dep_data$lhs, times = lens)
    rhs_rep <- rep(dep_data$rhs, times = lens)
    # flatten and coerce to numeric (NA introduced for empty entries, if any)
    lag_vec <- as.numeric(unlist(lags_list))
    # build explicit base data.frame
    if (length(lag_vec) == 0L) {
      # no lags -> empty data.frame with expected columns
      dep_data <- data.frame(lhs = character(0), rhs = character(0), lag = numeric(0),
                             stringsAsFactors = FALSE)
    } else {
      dep_data <- data.frame(lhs = lhs_rep, rhs = rhs_rep, lag = lag_vec,
                             stringsAsFactors = FALSE, row.names = NULL)
      # sort by lag descending (0, -1, -2, ...)
      dep_data <- dep_data[order(-dep_data$lag, dep_data$lhs, dep_data$rhs), , drop = FALSE]
      rownames(dep_data) <- NULL
    }
  }

  return(dep_data)
}
