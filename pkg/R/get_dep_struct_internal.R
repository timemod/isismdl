# internal function to create the dependency structure from the model_text
#' @useDynLib isismdl gen_dep_file
#' @importFrom stringr str_split
#' @importFrom zoo na.locf
get_dep_struct_internal <- function(model_text) {

  mdl_file_tmp1 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  mdl_file_tmp2 <- tempfile(pattern = "isismdl_", fileext = ".mdl")
  dep_file_tmp <- tempfile(pattern = "isismdl_", fileext = ".dep")

  writeLines(model_text, mdl_file_tmp1)

  #
  # substitute user functions, the dependency structure cannot be determined for
  # models with user functiions
  #
  ok <- .Call(convert_mdl_file_c, mdl_file_tmp1, mdl_file_tmp2, NULL,
              NULL, list(substitute = TRUE))
  stopifnot(ok)

  retval <- .Call(gen_dep_file, mdl_file_tmp2, dep_file_tmp)

  lines <- readLines(dep_file_tmp)

  parts <- stringr::str_split(lines, pattern = "\\s+", n = 3)

  lhs_names <- sapply(parts, FUN = function(x) x[1])
  lhs_names[lhs_names == ""] <- NA
  lhs_names <- na.locf(lhs_names)

  rhs_names <- sapply(parts, FUN = function(x) x[2])
  lags <- sapply(parts, FUN = function(x) x[3])

  df <- data.frame(rhs_name = rhs_names,
                   lags = lags)
  l <- split(df, lhs_names)

  fix_dep <- function(x) {
    rhs_names <- x$rhs_name
    lags <- str_split(trimws(x$lags), pattern = "\\s+")
    lags <- lapply(lags, FUN = as.numeric)
    names(lags) <- rhs_names
    return(lags)
  }

  result <- lapply(l, FUN = fix_dep)

  dum <- file.remove(c(mdl_file_tmp1, mdl_file_tmp2, dep_file_tmp))

  return(result)
}
