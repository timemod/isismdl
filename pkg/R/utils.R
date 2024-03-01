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
  a_word <- if (startsWith(type, "endo")) "an" else "a"
  problem_names <- setdiff(names, correct_names)
  n <- length(problem_names)
  if (n == 0) return(invisible())
  error_fun <- if (is_warning) warning else stop
  if (n == 1) {
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
