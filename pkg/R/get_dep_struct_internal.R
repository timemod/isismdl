# internal function to create the dependency structure from the model_text
#' @importFrom utils read.csv
#' @useDynLib isismdl gen_dep_file
get_dep_struct_internal <- function(model_text) {

  mdl_file_tmp1 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  mdl_file_tmp2 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  dep_file_tmp  <- tempfile(pattern = "isismdl_", fileext = ".dep")

  writeLines(model_text, mdl_file_tmp1)

  # substitute user functions, the dependency structure cannot be determined for
  # models with user functiions
  #
  ok <- .Call(convert_mdl_file_c, mdl_file_tmp1, mdl_file_tmp2, NULL,
              NULL, list(substitute = TRUE))
  stopifnot(ok)

  retval <- .Call(gen_dep_file, mdl_file_tmp2, dep_file_tmp)

  dep_data <- read.csv(dep_file_tmp, header = FALSE,
                       col.names = c("lhs",  "rhs", "lags"))

  dep_data$lags <- trimws(dep_data$lags, which = "right")

  dum <- file.remove(c(mdl_file_tmp1, mdl_file_tmp2, dep_file_tmp))

  return(dep_data)
}
