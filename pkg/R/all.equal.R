#' Test if two \code{\link{IsisMdl}} objects are (nearly) equal
#'
#' \code{all.equal(x, y)} is a utility to compare R objects x and y testing
#' near equality. If they are different, comparison is still made to some
#' extent, and a report of the differences is returned. Do not use
#' \code{all.equal} directly in if expressions - use
#' \code{isTRUE(all.equal(...))}.
#'
#' @usage
#' \method{all.equal}{IsisMdl}(target, current, ...)
#' @details
#'
#' The implementation of \code{all.equal} for \code{IsisMdl} objects
#' first serialized the model using the \code{IsisMdl} method
#' \code{serialize} and then uses \code{\link[base]{all.equal}} of
#' the base package.
#' @param target and \code{IsisMdl} object
#' @param current another \code{IsisMdl} object, to be compared with
#' \code{target}
#' @param  ... Arguments passed to the internal call of
#' \code{\link[base]{all.equal}}.
#' @return Either \code{TRUE} or a character vector describing the
#' differences between target and current.
#' @seealso \code{\link[base]{all.equal}}
#' @examples
#' mdl <- islm_mdl("2017Q2/2018Q2")
#' mdl2 <- mdl$copy()
#' print(all.equal(mdl, mdl2))
#'
#' # now modify mdl2
#' mdl2$set_values(600, names = "c")
#' print(all.equal(mdl, mdl2))
#' @export
#' @name all.equal
#' @aliases all.equal all.equal.IsisMdl
all.equal.IsisMdl <- function(target, current, ...) {
  target <- target$serialize()
  if (!inherits(current, "IsisMdl")) {
    stop("the second object compared is not an IsisMdl object")
  }
  current <- current$serialize()
  return(NextMethod(.Generic))
}
