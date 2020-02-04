
#' Print an object of class \code{tmleplus}
#'
#' @param x an object of class \code{tmleplus}
#' @export
print.tmleplus <- function(x) {
  print(x$estimates)
}
