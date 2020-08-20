# check the value passed to function set_values, set_rms_values, etc.
check_value <- function(value) {
  err_msg <- "Argument 'value' should be a scalar numeric"
  if (length(value) != 1) stop(err_msg)
  if (is.logical(value) && is.na(value)) {
    return(NA_real_)
  } else if (!is.numeric(value)) {
    stop(err_msg)
  } else {
    return(value)
  }
}
