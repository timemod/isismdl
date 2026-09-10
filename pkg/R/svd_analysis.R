#' Analyze linear dependencies among rows/columns of a matrix via SVD
#'
#' Rows of A that are linearly dependent show up as left singular vectors
#' belonging to (near-)zero singular values; dependent columns show up as
#' right singular vectors belonging to (near-)zero singular values.
#'
#' Rows of both vector matrices in which every entry is (absolutely) smaller
#' than \code{row_tol} are discarded, because they correspond to variables
#' that take negligible part in any dependency relation.
#'
#' @param A         numeric matrix (m x n, may be non-square), for example
#'                   a solve of fit Jacobian.
#' @param sv_tol    singular values below this count as "zero" (default the
#'                  square_root of the machine precision).
#' @param row_tol   threshold for "negligible" entries in the singular
#'                  vectors (default the square root of the machine precision).
#' @param ...       (unused, present for S3 method compatibility)
#'
#' @return an object of class \code{svd_analysis} (a list, invisibly) with
#' elements:
#'   - `A`                 the input matrix
#'   - `d`                  all singular values
#'   - `rank`               numerical rank of A
#'   - `small_sv`           the "zero" singular values themselves
#'   - `U_small`            cleaned left singular vectors  (m x k)
#'   - `V_small`            cleaned right singular vectors (n x k)
#'   - `lin_dep_rows`       row names or indices of the rows that participate in a dependency relation
#'   - `lin_dep_cols`       column names or indices of the colums that participate in a dependency relation
#'   - `sv_dtol`, `row_tol` the settings used.
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
svd_analysis <- function(A, sv_tol = sqrt(.Machine$double.eps),
                         row_tol = sqrt(.Machine$double.eps), ...) {

  if (!is.matrix(A)) A <- as.matrix(A)
  if (!is.numeric(A)) stop("svd_analysis(): 'A' must be a numeric matrix")

  r_names <- rownames(A)
  c_names <- colnames(A)

  m <- nrow(A)
  n <- ncol(A)
  k <- min(m, n)

  s <- svd(A)

  # svd() only returns nu = min(m,n) left vectors and nv = min(m,n) right
  # vectors
  d <- s$d    # vector of length k
  U <- s$u    # m x k
  V <- s$v    # n x k

  # --- select the vectors belonging to ~0 singular values -----------------
  idx_small <- which(d < sv_tol)
  rank <- sum(d >= sv_tol)

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
        sv_tol       = sv_tol,
        row_tol      = row_tol
      ),
      class = "svd_analysis"
    ))
  }

  U_small <- U[, idx_small, drop = FALSE]      # m x (# of zero sv)
  V_small <- V[, idx_small, drop = FALSE]      # n x (# of zero sv)
  rownames(U_small) <- r_names
  rownames(V_small) <- c_names
  small_sv <- d[idx_small]

  # --- remove rows in which ALL absolute values are below row_tol ---------
  # Columns of U_small give the linear relations among the ROWS of A;
  # columns of V_small give the linear relations among the COLUMNS of A.
  # A row consisting solely of ~0 entries means that this row/column of A
  # plays no role in any dependency relation.
  keepU <- apply(abs(U_small), 1, max) >= row_tol
  keepV <- apply(abs(V_small), 1, max) >= row_tol

  U_small_clean <- U_small[keepU, , drop = FALSE]
  V_small_clean <- V_small[keepV, , drop = FALSE]

  lin_dep_rows <- which(keepU)
  lin_dep_cols <- which(keepV)

  structure(
    list(
      A            = A,
      d            = d,
      rank         = rank,
      small_sv     = small_sv,
      U_small      = U_small_clean,
      V_small      = V_small_clean,
      lin_dep_rows = lin_dep_rows,
      lin_dep_cols = lin_dep_cols,
      sv_tol       = sv_tol,
      row_tol      = row_tol
    ),
    class = "svd_analysis"
  )
}

# -------------------------------------------------------------------------
# S3 print method for class "svd_analysis"
# -------------------------------------------------------------------------
#' @export
print.svd_analysis <- function(x, digits = 6, ...) {

  r_names <- rownames(x$A)
  c_names <- colnames(x$A)

  m <- nrow(x$A)
  n <- ncol(x$A)

  cat("== SVD analysis of a", m, "x", n, "matrix ==\n")
  cat("Singular values:", x$d, "\n")
  cat("Rank =", x$rank, "; number of small singular values (<", x$sv_tol, "):",
      length(x$small_sv), "\n\n")

  if (length(x$small_sv) == 0) {
    cat("Full numerical rank: no linear dependencies found.\n")
    return(invisible(x))
  }

  cat("-- Left singular vectors with sv <", x$sv_tol, " (row relations);\n",
      "   rows in which ALL |values| < ", x$row_tol, " were removed --\n", sep = "")
  print(round(x$U_small, digits))

  cat("\n-- Right singular vectors with sv <", x$sv_tol, " (column relations);\n",
      "   rows in which ALL |values| < ", x$row_tol, " were removed --\n", sep = "")
  print(round(x$V_small, digits))

  cat("\nInterpretation:\n")
  cat(" * Left singular vectors  = linear dependencies among the ROWS of A.\n")
  cat(" * Right singular vectors = linear dependencies among the COLUMNS of A.\n")
  cat(" * Rows with only ~0 entries were removed (not involved in any relation).\n")
  if (length(x$lin_dep_rows) > 0) {
    rows <- if (is.null(r_names)) x$lin_dep_rows else r_names[x$lin_dep_rows]
    cat(" * Rows    of A involved in a dependency:", rows, "\n")
  }
  if (length(x$lin_dep_cols) > 0) {
    cols <- if (is.null(c_names)) x$lin_dep_cols else c_names[x$lin_dep_cols]
    cat(" * Columns of A involved in a dependency:", cols, "\n")
  }

  invisible(x)
}

