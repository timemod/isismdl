#' Convert the simerr code to a string describing the solve status for method solve()
#'
#' simerr is an integer that contains the status code for the last solve()
#' attempt. This function converts it to a string describing the solve status.
#'
#' The error messages should agree with the messages in Fortran
#' subroutine report_solve_error in msimul.f90.
#'
#' @param simerr An integer, the return code of C function solve_c
#'
#' @returns Character: the solve status as string.
#' @noRd
simerr_to_solve_status <- function(simerr) {

  status_txts <- c(
    "-1" = "Method solve has not yet been called",
    "0" = "OK",
    "1" = "Simulation not possible",
    "2" = "Simulation stopped",
    "3" = "Initial lags/leads missing/invalid. Simulation not possible",
    "4" = "Invalid parameter values detected. Simulation not possible",
    "5" = "Fair-Taylor has not converged",
    "6" = "Out of memory. Simulation not succesful"
  )

  solve_status_txt <- status_txts[[as.character(simerr)]]
  if (is.na(solve_status_txt)) {
    solve_status_txt <- "Unknown problem in solve. Simulation not succesfull"
  }

  return(solve_status_txt)
}
