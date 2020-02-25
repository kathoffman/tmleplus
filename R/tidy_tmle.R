
#' Title
#'
#' @param fit
#' @param obs_a
#' @param obs_y
#'
#' @return
#' @export
#'
#' @examples
#' set.seed(1)
#' n <- 250
#' W <- matrix(rnorm(n*3), ncol=3)
#' A <- rbinom(n,1, 1/(1+exp(-(.2*W[,1] - .1*W[,2] + .4*W[,3]))))
#' Y <- A + 2*W[,1] + W[,3] + W[,2]^2 + rnorm(n)
#' tmle_fit <- tmle::tmle(Y,A,W, Q.SL.library = "SL.glm", g.SL.library = "SL.glm")
#' tidy_tmle(tmle_fit, A, Y)
tidy_tmle.tmle <- function(fit, a = NULL, y = NULL) {

  tmle_fit <- fit
  oob_estimates <- tmle_fit$estimates[oob_tmle_estimates]
  available_oob <- names(lapply(oob_estimates, is.null))[!unlist(lapply(oob_estimates, is.null))]
  oob <- purrr::map_dfr(oob_estimates[available_oob], out_of_box, .id = "parameter")

  if (is.null(a)) {
    if (is.null(y)) {
      # need to access influence curve info for the ATE, ATT, and ATC when a and y are NULL
      # likely requires rewriting get_IF
    }
    out <- list(estimates = oob,
                IF = tmle_fit$estimates$IC$IC.EY1)
    return(out)
  }

  if (is.null(a)) {
    if (is.null(y)) {

    }
    infer <- cbind(data.frame(parameter = "EY1"), out_of_box.tmle(tmle_fit$estimates$EY1))
    out <- list(estimates = infer,
                IF = tmle_fit$estimates$IC$IC.EY1)
    class(out) <- "tmleplus"
    return(out)
  }

  g1w <- tmle_fit$g$g1W
  g0w <- 1 - g1w
  Q1W <- tmle_fit$Qstar[, 2]
  Q0W <- tmle_fit$Qstar[, 1]
  use <- data.frame(A = a,
                    Y = obs_y,
                    g1w = g1w,
                    g0w = g0w,
                    Q1W = Q1W,
                    Q0W = Q0W,
                    ATE = Q1W - Q0W)

  if (!is.null(tmle_fit$estimates$RR)) {
    oob <- purrr::map_dfr(tmle_fit$estimates[c("ATT", "ATC", "RR", "OR")], out_of_box, .id = "parameter")
  } else {
    oob <- purrr::map_dfr(tmle_fit$estimates[c("ATT", "ATC")], out_of_box, .id = "parameter")
  }

  IF <- get_IF.tmle(use)
  infer <- tmle_inference.tmle(IF)

  out <- list(estimates = rbind(infer, oob),
              IF = IF)

  class(out) <- "tmleplus"
  return(out)
}

get_IF <- function(data) {
  a <- data[["A"]]
  y <- data[["Y"]]
  g <- as.list(data[, c("g1w", "g0w")])
  q <- as.list(data[, c("Q1W", "Q0W")])
  i <- c(1, 0)

  build_IF <- function(i., g., q., a. = a, y. = y) {
    theta <- q.
    theta[a. == i.] <- theta[a. == i.] + (y.[a. == i.] - q.[a. == i.])/g.[a. == i.]
    theta <- theta - mean(theta)
    return(theta)
  }

  IF <- purrr::pmap(list(i. = i, g. = g, q. = q), build_IF)
  # dont really need this when we already have it calculated for us
  ate_IF <- IF[[1]] - IF[[2]]
  out <- data.frame(a1_IF = IF[[1]],
                    a0_IF = IF[[2]],
                    ate_IF = ate_IF)
  cbind(data, out)
}

out_of_box.tmle <- function(est) {
  mu <- est$psi
  variance <- if (is.null(est$var.psi)) est$var.log.psi else est$var.psi
  se <- sqrt(variance)
  z <- if (is.null(est$log.psi)) mu / se else log(mu) / se
  p <- est$pvalue
  conf.low <- est$CI[1]
  conf.high <- est$CI[2]

  data.frame(estimate = mu,
             variance = variance,
             standard_error = se,
             z = z,
             p = p,
             conf.low = conf.low,
             conf.high = conf.high)
}

inference.tmle <- function(qstar, IF) {
  mu <- mean(qstar)
  variance <- var(IF) / length(IF)
  se <- sqrt(variance)
  z <- mu / se
  er <- abs(qnorm(0.025)) * se
  ci <- c(mu - er, mu + er)

  data.frame(estimate = mu,
             variance = variance,
             standard_error = se,
             z = z,
             p = 2 * pnorm(abs(z), lower.tail = F),
             conf.low = ci[1],
             conf.high = ci[2])
}

tmle_inference.tmle <- function(data) {
  subs <- as.list(data[, c("Q1W", "Q0W", "ATE")])
  ifs <- as.list(data[, c("a1_IF", "a0_IF", "ate_IF")])

  purrr::map2_dfr(subs, ifs, inference.tmle, .id = "parameter")
}


