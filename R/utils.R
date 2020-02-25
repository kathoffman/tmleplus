
#' Print an object of class \code{tmleplus}
#'
#' @param x an object of class \code{tmleplus}
#' @export
print.tmleplus <- function(x) {
  print(x$estimates)
}

oob_tmle_estimates <- c(c("EY1", "ATT", "ATC", "RR", "OR"))

check_package_loaded <- function(pkg) {
  tryCatch(
    if (isNamespaceLoaded(pkg)) {
      on.exit()
    } else {
      has_ltmle <- requireNamespace(pkg)
      if (isFALSE(has_ltmle)) stop()
      else on.exit()
    }, error = function(e) {
      stop(paste("Couldn't find package", pkg))
    }
  )
}
