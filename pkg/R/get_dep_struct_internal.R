# internal function to create the dependency structure from the model_text
#' @importFrom utils read.csv
get_dep_struct_internal <- function(model_text, active_endo_names) {

  mdl_file_tmp1 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  mdl_file_tmp2 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  dep_file_tmp  <- tempfile(pattern = "isismdl_", fileext = ".dep")

  writeLines(model_text, mdl_file_tmp1)

  # substitute user functions, the dependency structure cannot be determined for
  # models with user functiions
  #
  ok <- .Call(C_convert_mdl_file_c, mdl_file_tmp1, mdl_file_tmp2, NULL,
              NULL, list(substitute = TRUE))
  stopifnot(ok)

  ok <- .Call(C_gen_dep_file, mdl_file_tmp2, dep_file_tmp)
  stopifnot(ok)

  dep_data <- read.csv(dep_file_tmp, header = FALSE,
                       col.names = c("lhs",  "rhs", "lags"))

  #
  # remove dependency of inactive equations
  #
  row_sel <- dep_data$lhs %in% active_endo_names
  dep_data <- dep_data[row_sel, , drop = FALSE]

  # sort the lhs names alphabetically. The ordering of the result returned
  # by gen_dep_file appears to be completely random (the ordering is based on
  # the hash code)
  dep_data <- dep_data[order(dep_data$lhs), , drop = FALSE]
  rownames(dep_data) <- NULL

  file.remove(c(mdl_file_tmp1, mdl_file_tmp2, dep_file_tmp))

  return(dep_data)
}
