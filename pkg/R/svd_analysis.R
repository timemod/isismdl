#' Analyze linear dependencies among rows/columns of a matrix via SVD
#'
#' Rows of A that are linearly dependent show up as left singular vectors
#' belonging to (near-)zero singular values; dependent columns show up as
#' right singular vectors belonging to (near-)zero singular values.
#' This function reports these singular vectors, which may help to identify
#' (near) linear dependencies among rows and columns.
#' \cr\cr
#' The function also reports all rows and columns that only contain near-zero
#' entries. These zero rows and columns are trivially dependent, but
#' are usually worth a separate warning. Note that while a zero row or
#' column always implies rank loss for a square matrix, this is not the case
#' for a non-square matrix.
#' \cr\cr
#' The function uses three tolerances, each with its own scope:
#' \describe{
#'   \item{\code{sv_tol}}{applied to the singular values of A: singular
#'     values below \code{sv_tol} are treated as zero. The number of
#'     singular values >= \code{sv_tol} is the numerical rank of A.}
#'   \item{\code{vector_tol}}{applied to the entries of the singular
#'     vectors belonging to the ~zero singular values: rows of these
#'     vector matrices in which every entry has absolute value below
#'     \code{vector_tol} are discarded, because they correspond to
#'     rows/columns of A that play no role in any dependency relation.}
#'   \item{\code{zero_tol}}{applied to the entries of A itself:
#'     rows and columns of A in which every entry has absolute value below
#'     \code{zero_tol} (all-zero rows/columns) are reported separately
#'     as all-zero rows and columns.}
#' }
#'
#' If A has no row names or column names, the print method reports
#' row/column numbers instead.
#'
#' @param A          numeric matrix (m x n, may be non-square), for example
#'                   a solve or fit Jacobian.
#' @param sv_tol     singular values below this count as "zero"
#'                   (default the square root of the machine precision).
#' @param vector_tol threshold for "negligible" entries in the singular
#'                   vectors: rows of the singular-vector matrices in
#'                   which ALL absolute values are below \code{vector_tol}
#'                   are discarded
#'                   (default the square root of the machine precision).
#' @param zero_tol   threshold for negligible entries of A itself:
#'                   rows and columns of A with maximum absolute value
#'                   below \code{zero_tol} are reported as all-zero
#'                   rows/columns
#'                   (default the square root of the machine precision).

#' @param ...        (unused, present for S3 method compatibility)
#'
#' @return an object of class \code{svd_analysis} (a list, invisibly) with
#' elements:
#'   - `A`              the input matrix (m x n)
#'   - `d`              all singular values (a vector of length min(m, n))
#'   - `rank`           numerical rank of A
#'   - `small_sv`       the (near)-zero singular values
#'   - `U_small`        cleaned left singular vectors  (n x length(small_sv))
#'   - `V_small`        cleaned right singular vectors (n x length(small_sv))
#'   - `lin_dep_rows`   indices of rows participating in a dependency
#'   - `lin_dep_cols`   indices of columns participating in a dependency
#'   - `zero_rows`      indices of all-(near-)zero rows of A
#'   - `zero_cols`      indices of all-(near-)zero columns of A
#'   - `sv_tol`, `vector_tol`, `zero_tol` the specified tolerances
#'
#' @examples
#'
#' mdl <- islm_mdl("2020Q1")
#' y <- regts(985, start = "2020q1")
#' yd <- regts(800, start = "2020q1")
#' c <- regts(600, start = "2020q1")
#' fit <- cbind(y, yd, c)
#' mdl$set_fit(fit)
#' mdl$set_rms(c(c = 5.0, i = 21, md = 2))
#' mdl$set_fit_options(store_jac = TRUE)
#' mdl$solve()
#' fit_jac <- mdl$get_fit_jac()
#' print(fit_jac)
#' svd_analysis(fit_jac)
#' @export
#' @seealso \code{\link{get_solve_jac}}, \code{\link{get_fit_jac}}
#' and \code{\link{get_jacobian}}.
svd_analysis <- function(A, sv_tol = sqrt(.Machine$double.eps),
                         vector_tol = sqrt(.Machine$double.eps),
                         zero_tol = sqrt(.Machine$double.eps),
                         ...) {
  if (!is.matrix(A)) A <- as.matrix(A)
  if (!is.numeric(A)) stop("svd_analysis(): 'A' must be a numeric matrix")

  s <- svd(A)
  # If A is m x n matrix, svd() always returns k = min(m,n) left vectors
  # (m x k) and k right vectors (n x k), so each row of U corresponds to a
  # row of A and each row of V to a column of A.
  d <- s$d    # vector of length k
  U <- s$u    # m x k
  V <- s$v    # n x k

  # --- select the vectors belonging to ~0 singular values -----------------
  idx_small <- which(d < sv_tol)
  rank <- sum(d >= sv_tol)

  # --- all-(near-)zero rows and columns of A itself ------------------------
  zero_rows <- unname(which(apply(abs(A), 1, max) < zero_tol))
  zero_cols <- unname(which(apply(abs(A), 2, max) < zero_tol))

  if (length(idx_small) == 0) {
    # Full numerical rank: no dependencies to report
    return(structure(
      list(
        A            = A,
        d            = d,
        rank         = rank,
        small_sv     = numeric(0),
        U_small      = NULL,
        V_small      = NULL,
        lin_dep_rows = integer(0),
        lin_dep_cols = integer(0),
        zero_rows    = zero_rows,
        zero_cols    = zero_cols,
        sv_tol       = sv_tol,
        vector_tol   = vector_tol,
        zero_tol     = zero_tol
      ),
      class = "svd_analysis"
    ))
  }

  U_small <- U[, idx_small, drop = FALSE]      # m x (# of zero sv)
  V_small <- V[, idx_small, drop = FALSE]      # n x (# of zero sv)
  rownames(U_small) <- rownames(A)
  rownames(V_small) <- colnames(A)
  small_sv <- d[idx_small]

  # --- remove rows in which ALL absolute values are below vector_tol ---------
  # Columns of U_small give the linear relations among the ROWS of A;
  # columns of V_small give the linear relations among the COLUMNS of A.
  # A row consisting solely of ~0 entries means that this row/column of A
  # plays no role in any dependency relation.
  keepU <- apply(abs(U_small), 1, max) >= vector_tol
  keepV <- apply(abs(V_small), 1, max) >= vector_tol

  U_small_clean <- U_small[keepU, , drop = FALSE]
  V_small_clean <- V_small[keepV, , drop = FALSE]

  structure(
    list(
      A            = A,
      d            = d,
      rank         = rank,
      small_sv     = small_sv,
      U_small      = U_small_clean,
      V_small      = V_small_clean,
      lin_dep_rows = unname(which(keepU)),
      lin_dep_cols = unname(which(keepV)),
      zero_rows    = zero_rows,
      zero_cols    = zero_cols,
      sv_tol       = sv_tol,
      vector_tol   = vector_tol,
      zero_tol     = zero_tol
    ),
    class = "svd_analysis"
  )
}

# -------------------------------------------------------------------------
# S3 print method for class "svd_analysis"
# -------------------------------------------------------------------------
#' @export
print.svd_analysis <- function(x, digits = getOption("digits"), ...) {

  m <- nrow(x$A)
  n <- ncol(x$A)

  r_names <- rownames(x$A)
  c_names <- colnames(x$A)

  sv_tol_rounded <- signif(x$sv_tol, digits = digits)
  vector_tol_rounded <- signif(x$vector_tol, digits = digits)

  cat("== SVD analysis of a", m, "x", n, "matrix ==\n")
  cat("\nRank = ", x$rank, "; number of small singular values (< ",
      sv_tol_rounded, "): ", length(x$small_sv), "\n", sep = "")
  cat("\nSmallest Singular Value: ", signif(min(x$d), digits = digits)
      , "\n", sep = "")

  if (length(x$small_sv) == 0) {
    if (length(x$zero_rows) > 0 || length(x$zero_cols) > 0) {
      cat("\nRows/cols with only zero values:\n")
    }
    report_zero_rows_cols(x, r_names, c_names, digits = digits)
    cat("Full numerical rank: no linear dependencies found.\n")

    return(invisible(x))
  }

  cat("\n-- Singular values with sv < ", sv_tol_rounded, " --\n", sep = "")
  print(x$small_sv, digits = digits)

  cat("\n-- Left singular vectors with sv < ", sv_tol_rounded, " (row relations);\n",
      "   rows in which ALL |values| < ", vector_tol_rounded, " were removed --\n",
      sep = "")
  print(x$U_small, digits = digits)

  cat("\n-- Right singular vectors with sv < ", sv_tol_rounded, " (column relations);\n",
      "   rows in which ALL |values| < ", vector_tol_rounded, " were removed --\n",
      sep = "")
  print(x$V_small, digits = digits)

  cat("\nInterpretation:\n")
  cat(" * Left singular vectors  = linear dependencies among the ROWS of A.\n")
  cat(" * Right singular vectors = linear dependencies among the COLUMNS of A.\n")
  if (length(x$lin_dep_rows) > 0) {
    cat(" * Rows    of A involved in a dependency:\n")
    report_rows_or_cols(x$lin_dep_rows, r_names)
  }
  if (length(x$lin_dep_cols) > 0) {
    cat(" * Columns of A involved in a dependency:\n")
    report_rows_or_cols(x$lin_dep_cols, c_names)
  }

  report_zero_rows_cols(x, r_names, c_names, digits = digits)
  invisible(x)
}

# helper: report row/column names or indices.
# indices is a numerical vector with row or column indices.
# rc_names is a character vector with row or column names (may be NULL)
report_rows_or_cols <- function(indices, rc_names) {
  rc_names <- if (is.null(rc_names)) indices else rc_names[indices]
  names_list <- paste0(paste(rc_names, collapse = ", "), ".")
  writeLines(strwrap(names_list, width = 80, indent = 6, exdent = 6))
}

# helper: report all-(near-)zero rows and columns of A
report_zero_rows_cols <- function(x, r_names, c_names, digits) {
  output <- FALSE
  zero_tol_rounded <- signif(x$zero_tol, digits = digits)
  if (length(x$zero_rows) > 0) {
    cat(" * Rows    of A with only zero values (< ", zero_tol_rounded, "):\n",
        sep = "")
    report_rows_or_cols(x$zero_rows, r_names)
    output <- TRUE
  }
  if (length(x$zero_cols) > 0) {
    cat(" * Columns of A with only zero values (< ", zero_tol_rounded, "):\n",
        sep = "")
    report_rows_or_cols(x$zero_cols, c_names)
    output <- TRUE
  }
  if (output) cat("\n")
}
