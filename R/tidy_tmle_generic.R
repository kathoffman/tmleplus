
#' Tidy a TMLE fit
#'
#' @param fit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
tidy_tmle <- function(fit, ...) {
  UseMethod("fit")
}
