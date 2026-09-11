library(testthat)
library(isismdl)

update_expected <- FALSE

test_that("svd_analysis for non singular matrix", {
  A <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2, byrow = TRUE)
  rownames(A) <- c("r1", "r2")
  colnames(A) <- c("c1", "c2")

  result <- svd_analysis(A)

  expect_equal(
    result,
    structure(
      list(
        A            = A,
        d            = svd(A)$d,
        rank         = 2,
        small_sv     = numeric(0),
        U_small      = NULL,
        V_small      = NULL,
        lin_dep_rows = integer(0),
        lin_dep_cols = integer(0),
        zero_rows    = integer(0),
        zero_cols    = integer(0),
        sv_tol       =  sqrt(.Machine$double.eps),
        vector_tol   =  sqrt(.Machine$double.eps),
        zero_tol     =  sqrt(.Machine$double.eps)
      ),
      class = "svd_analysis"
    )
  )
})

test_that("non-square matrix with two nearly singular values", {
  # Create a 4x3 matrix where rows are linearly dependent producing small singular values
  # Row2 = 2*Row1, Row3 = 3*Row1, so rank is 1 (min(4,3)=3, so 2 zero singular values)
  A <- matrix(c(
    1, 2, 3,
    2, 4, 6,
    3, 6, 9,
    0, 0, 0
  ), nrow = 4, ncol = 3, byrow = TRUE)

  rownames(A) <- c("r1", "r2", "r3", "r4")
  colnames(A) <- c("c1", "c2", "c3")

  result <- svd_analysis(A)

  expect_equal(result$A, A)
  expect_equal(result$rank, 1)
  expect_true(length(result$small_sv) == 2)
  expect_equal(dim(result$U_small), c(3, 2))
  expect_equal(dim(result$V_small), c(3, 2))
  expect_equal(result$lin_dep_rows, 1:3)
  expect_equal(result$lin_dep_cols, 1:3)
  expect_equal(result$zero_rows, 4)
  expect_equal(length(result$zero_cols), 0)
})

test_that("vector_tol", {
  # Create a 4x3 matrix where rows are linearly dependent producing small singular values
  # Row2 = 2*Row1, Row3 = 3*Row1, so rank is 1 (min(4,3)=3, so 2 zero singular values)
  A <- matrix(c(
    1, 2, 3,
    2, 4, 6,
    3, 6, 9,
    0, 0, 0
  ), nrow = 4, ncol = 3, byrow = TRUE)

  rownames(A) <- c("r1", "r2", "r3", "r4")
  colnames(A) <- c("c1", "c2", "c3")

  result <- svd_analysis(A, vector_tol = 1)

  expect_equal(result$A, A)
  expect_equal(result$rank, 1)
  # Matrix has rank 1, so there should be 2 small singular values (min(4,3)-1 = 2)
  expect_true(length(result$small_sv) >= 2)
  expect_equal(dim(result$U_small), c(0, 2))
  expect_equal(dim(result$V_small), c(0, 2))
  expect_equal(length(result$lin_dep_rows), 0)
  expect_equal(length(result$lin_dep_cols), 0)
  expect_equal(result$zero_rows, 4)
  expect_equal(length(result$zero_cols), 0)
})

test_that("svd_analysis throws error for non-numeric input", {
  A <- matrix(c("a", "b", "c", "d"), nrow = 2, ncol = 2)

  expect_error(svd_analysis(A), "svd_analysis\\(\\): 'A' must be a numeric matrix")
})

test_that("svd_analysis throws error for non-matrix input that cannot be coerced to numeric", {
  A <- list(a = 1, b = 2)

  expect_error(svd_analysis(A), "svd_analysis\\(\\): 'A' must be a numeric matrix")
})

test_that("svd_analysis throws error for matrix with 0 rows and columns", {
  A <- matrix(numeric(0), nrow = 0, ncol = 0)

  expect_error(svd_analysis(A), "a dimension is zero")
})
