#' Get CIs for survtmle object or any other object without auto-calculated CIs
#'
#' @param est Estimate of an effect
#' @param se Standard error of an effect
#'
#' @return a data frame with the estimate,standard error, p-value and 95% CIs
#' @export
#'
#' @examples
getCI <- function(est, se){
  conf_low <- est - 1.96*se
  conf_high <- est + 1.96*se
  p <- 2 * pnorm(abs(est) / se, lower.tail = FALSE)
  out <- data.frame(est, se, p, conf_low, conf_high)
  return(out)
}


#' Tidy an S3 object of class survtmle
#'
#' @param fit An object of type survtmle
#'
#' @return a tibble or dataframe of class tmleplus
#' @export
#'
#' @examples
#'
#' trt
#' The numeric vector of treatment assignments used in the fit.
#'
#' adjustVars
#' The data.frame of failure times used in the fit.
#'
#' Examples
#'
#' library(survtmle)
#' # simulate data
#' set.seed(1234)
#' n <- 200
#' trt <- rbinom(n, 1, 0.5)
#' adjustVars <- data.frame(W1 = round(runif(n)), W2 = round(runif(n, 0, 2)))
#'
#' ftime <- round(1 + runif(n, 1, 4) - trt + adjustVars$W1 + adjustVars$W2)
#' ftype <- round(runif(n, 0, 1))
#'
#' # fit a survtmle object with glm estimators for treatment, censoring, and
#' # failure using the "mean" method
#' fit1 <- survtmle(ftime = ftime, ftype = ftype,
#'                  trt = trt, adjustVars = adjustVars,
#'                  glm.trt = "W1 + W2",
#'                  glm.ftime = "trt + W1 + W2",
#'                  glm.ctime = "trt + W1 + W2",
#'                  method = "mean", t0 = 6)
#' tidy_tmle(fit)
#'
#'
#'
tidy_tmle.survtmle <- function(fit){

  check_package_loaded("survtmle")

  # marginal cumulative incidences for trt and control
  MCI_0 <- fit$est[[1]]
  MCI_1 <- fit$est[[2]]
  # marginal cumulative incidence MCI_difference
  MCI_diff <- MCI_1 - MCI_0
  # estimates to vector
  est <- c(MCI_0,MCI_1,MCI_diff)
  # standard errors
  se_0 <- sqrt(fit$var[1,1])
  se_1 <- sqrt(fit$var[2,2])
  # standard error for difference, from ICs
  se_diff <- sd(fit$ic[, 2] - fit$ic[, 1]) / sqrt(nrow(fit$ic))
  # ses to se vector
  se <- c(se_0, se_1, se_diff)
  # get confidence intervals and p-values for inference
  inf <- purrr::map2_dfr(est, se, getCI)
  # add notation specific to survtmle (marginal cumulative incidences)
  effect <- c("MCI_A0", "MCI_A1", "MCID")

  out <- data.frame(effect, inf)
  class(out) <- "tmleplus"
  return(out)

}
