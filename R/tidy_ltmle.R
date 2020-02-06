
#' Extract estimates from a(n) ltmle fit
#'
#' @param fit an object of class ltmleEffectMeasures
#'
#' @return a tibble or dataframe of class tmleplus
#' @export
#'
#' @examples
#' rexpit <- function(x) rbinom(n=length(x), size = 1, prob = plogis(x))
#' n <- 1000
#' W1 <- rnorm(n)
#' W2 <- rbinom(n, size=1, prob=0.3)
#' W3 <- rnorm(n)
#' A <- rexpit(-1 + 2 * W1 + W3)
#' Y <- rexpit(-0.5 + 2 * W1^2 + 0.5 * W2 - 0.5 * A + 0.2 * W3 * A - 1.1 * W3)
#' data <- data.frame(W1, W2, W3, A, Y)
#' fit <- ltmle(data, Anodes="A", Lnodes=NULL, Ynodes="Y", abar=list(1, 0), SL.library="glm")
#' tidy_tmle(fit)
tidy_tmle.ltmleEffectMeasures <- function(fit) {

  tryCatch(
    if (isNamespaceLoaded("ltmle")) {
      on.exit()
    } else {
      has_ltmle <- requireNamespace("ltmle")
      if (isFALSE(has_ltmle)) stop()
      else on.exit()
    }, error = function(e) {
      stop("Couldn't find package 'ltmle'")
    }
  )

  fs <- summary(fit)
  estimates <- out_of_box(fs)

  out <- list(estimates = estimates)
  class(out) <- "tmleplus"
  return(out)
}

out_of_box.summary.ltmleEffectMeasures <- function(fs) {
  to_df <- function(fs) {
    data.frame(estimate = fs$estimate,
               standard_error = fs$std.dev,
               p = fs$p,
               conf.low = fs$CI[1],
               conf.high = fs$CI[2])
  }
  out <- purrr::map_dfr(fs$effect.measures, to_df, .id = "parameter")
  return(out)
}
