#' Calculate effect modification from survtmle fit objects
#' @description This calculations an effect modifier's difference and corresponding standard error for the cumulative hazard incidence using objects obtained from the `survtmle` package.
#' @param dat_full The full data set containing all observations used to obtain each `tmle_fit_1` and `tmle_fit_0`.
#' @param mod_var String containing column name of the effect modifying variable in `dat_full`.
#' @param tmle_fit_1 survtmle fit object for only patients with the effect modifier
#' @param tmle_fit_0 survtmle fit object for only patients without the effect modifier
#'
#' @return
#' @export
#'
#' @examples
#' # Modified version of the survtmle vignette example
#' set.seed(1234)
#' n <- 1000
#' t_0 <- 6
#' trt <- rbinom(n, 1, 0.5)
#' eff <- rbinom(n, 1, 0.5)
#' adjustVars <- data.frame(W1 = round(runif(n)), W2 = round(runif(n, 0, 2)))
#' ftime <- round(1 + runif(n, 1, 4) - trt*eff + adjustVars$W1 + adjustVars$W2)
#' ftype <- round(runif(n, 0, 1))
#' dat <- data.frame(trt, eff, adjustVars, ftime, ftype)
#'
#' dat_a <- dat[dat$eff == 0,]
#' trt <- dat_a$trt
#' adjustVars <- dat_a %>% select(W1, W2)
#' ftime <- dat_a$ftime
#' ftype <- dat_a$ftype
#' fit_a <- survtmle::survtmle(ftime = ftime, ftype = ftype,
#'                   trt = trt, adjustVars = adjustVars,
#'                   SL.trt = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ftime = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ctime = c("SL.glm", "SL.mean", "SL.step"),
#'                   method = "hazard", t0 = t_0)
#'
#'
#' dat_b <- dat[dat$eff == 1,]
#' trt <- dat_b$trt
#' adjustVars <- dat_b %>% select(W1, W2)
#' ftime <- dat_b$ftime
#' ftype <- dat_b$ftype
#' fit_b <- survtmle::survtmle(ftime = ftime, ftype = ftype,
#'                   trt = trt, adjustVars = adjustVars,
#'                   SL.trt = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ftime = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ctime = c("SL.glm", "SL.mean", "SL.step"),
#'                   method = "hazard", t0 = t_0)
#'
#' surv_effect_mod(dat_full = dat, mod_var = "eff", tmle_fit_0 = fit_a, tmle_fit_1 = fit_b)

surv_effect_mod <- function(tmle_fit_0, tmle_fit_1, dat_full, mod_var){

    if (length(c(tmle_fit_0$trt, tmle_fit_1$trt)) == nrow(dat_full)){

      ic_1 <- tmle_fit_1$ic[, 2] - tmle_fit_1$ic[, 1]
      ic_0 <- tmle_fit_0$ic[, 2] - tmle_fit_0$ic[, 1]

      dat_full[dat_full[mod_var] == 1, 'ic_1'] <- ic_1
      dat_full[is.na(dat_full$ic_1), 'ic_1'] <- 0
      dat_full[, 'ic_1'] <- dat_full[, mod_var] * dat_full[, 'ic_1'] /
        mean(dat_full[, mod_var])

      dat_full[dat_full[mod_var] == 0, 'ic_0'] <- ic_0
      dat_full[is.na(dat_full$ic_0), 'ic_0'] <- 0
      dat_full[, 'ic_0'] <- (1 - dat_full[, mod_var]) * dat_full[, 'ic_0'] /
        mean(1 - dat_full[, mod_var])

      se_1 <- sd(dat_full$ic_1) / sqrt(nrow(dat_full))
      se_0 <- sd(dat_full$ic_0) / sqrt(nrow(dat_full))

      seeffmod <- sd(with(dat_full, ic_1 - ic_0)) / sqrt(nrow(dat_full))

      effect_1 <- diff(tmle_fit_1$est[, 1])
      effect_0 <- diff(tmle_fit_0$est[, 1])

      effectmod <- effect_0 - effect_1

      effects <- rbind(c(effect_1, effect_0, effectmod),
                       c(se_1, se_0, seeffmod))

      colnames(effects) <- c('effect_1', 'effect_0', 'difference')
      rownames(effects) <- c('estimate', 's.e.')

      return(effects)}

    else {cat("Number of observations contained in the tmle fits and full data set differ. \nCheck that tmle_fit_0 and tmle_fit_1 together contain all observations in dat_full and vice versa.")}

  }

