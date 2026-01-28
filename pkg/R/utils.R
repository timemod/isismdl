# Utility function for error / warning messages: concate a number of names,
# separating the first n - 1 names with "," and the last with "and".
# Finally, "is" or "are" are added depending on the number of names.
# The function also surrounds the names with single quotes.
concat_names <- function(names) {
  n <- length(names)
  if (n == 0) return(names)
  names <- paste0("'", names, "'")
  if (n == 1) {
    return(names)
  } else {
    return(paste(paste(names[-n], collapse = ", "), "and", names[n]))
  }
}

# Check if the supplied names are existing model variables of the
# correct type.
check_names <- function(names, correct_names, type, is_warning = FALSE) {
  if (length(names) == 0) return(invisible())
  problem_names <- setdiff(names, correct_names)
  n <- length(problem_names)
  if (n == 0) return(invisible())
  error_fun <- if (is_warning) warning else stop
  if (n == 1) {
    a_word <- if (startsWith(type, "endo")) "an" else "a"
    error_fun("'", problem_names, "' is not ", a_word, " ", type, ".")
  } else {
    problem_names_text <- concat_names(problem_names)
    msg <- paste0("The following names are not ", type, "s:\n",
                  problem_names_text, ".")
    lines <- strwrap(msg, width = 80, exdent = 4)
    error_fun(paste(lines, collapse = "\n"))
  }
  return(invisible())
}

#' Checks whether a period range lies within the range of another period range.
#'
#' The two period ranges should have the same frequency (this is not checked).
#'
#' @param period A period range to check.
#' @param long_period A period range that may contain `period`.
#' @return A logical, `TRUE` if period range `period` lies within the range
#' `long_period`.
#' @noRd
period_range_is_within <- function(period, long_period) {
  start_period(period) >= start_period(long_period)  &&
    (end_period(period) <= end_period(long_period))
}


#' Fill in missing lower or upper bounds of a period range with bounds from
#' another period range.
#'
#' The two period ranges should have the same frequency (this is not checked).
#'
#' @param period A period range that may have missing (NULL) start or end bounds.
#' @param base_period A period range providing default values for any missing bounds.
#' @return A period range with all bounds defined, using `base_period` bounds
#' where `period` bounds are NULL.
#' @noRd
fill_missing_range_bounds <- function(period, base_period) {
  startp <- start_period(period)
  if (is.null(startp)) {
    startp <- start_period(base_period)
  }
  endp <- end_period(period)
  if (is.null(endp)) {
    endp <- end_period(base_period)
  }
  period_range(startp, endp)
}

#' Check whether a period range has missing bounds.
#'
#' @param period A period range to check.
#' @return A logical, `TRUE` if either the start or end bound is NA.
#' @noRd
has_missing_bounds <- function(period) {
  is.na(period[1]) || is.na(period[2])
}
