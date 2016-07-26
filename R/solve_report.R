#' @export
print.solve_report <- function(x, ...) {
    cat(paste("Success:", x$success, "\n"))
    cat(paste("Message:", x$message, "\n"))
    cat("\nSolve report:\n")
    cat("-------------\n")
    cat(x$full_report)
    cat("\n")
}
