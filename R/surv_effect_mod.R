#' Calculate effect modification from survtmle fit objects
#' @description This calculations an effect modifier's difference and corresponding standard error for the marginal cumulative incidence using objects obtained from the `survtmle` package.
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
#' # Run survtmle for all subjects without the effect modifier
#' dat_noeff <- dat[dat$eff == 0,]
#' trt <- dat_noeff$trt
#' adjustVars <- dat_noeff[c("W1", "W2")]
#' ftime <- dat_noeff$ftime
#' ftype <- dat_noeff$ftype
#' fit_noeff <- survtmle::survtmle(ftime = ftime, ftype = ftype,
#'                   trt = trt, adjustVars = adjustVars,
#'                   SL.trt = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ftime = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ctime = c("SL.glm", "SL.mean", "SL.step"),
#'                   method = "hazard", t0 = t_0)
#'
#' # Run survtmle for all subjects with the effect modifier
#' dat_eff <- dat[dat$eff == 1,]
#' trt <- dat_eff$trt
#' adjustVars <- dat_eff[c("W1", "W2")]
#' ftime <- dat_eff$ftime
#' ftype <- dat_eff$ftype
#' fit_eff <- survtmle::survtmle(ftime = ftime, ftype = ftype,
#'                   trt = trt, adjustVars = adjustVars,
#'                   SL.trt = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ftime = c("SL.glm", "SL.mean", "SL.step"),
#'                   SL.ctime = c("SL.glm", "SL.mean", "SL.step"),
#'                   method = "hazard", t0 = t_0)
#'
#' # Using the full data set, specify the effect modification column name as a string and tmle fits as 3rd and 4th args
#' surv_eff_mod(dat_full = dat, mod_var = "eff", tmle_fit_0 = fit_noeff, tmle_fit_1 = fit_eff)

surv_eff_mod <- function(tmle_fit_1, tmle_fit_0, dat_full, mod_var){

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

      effects <- cbind(c(effect_1, effect_0, effectmod),
                       c(se_1, se_0, seeffmod))

      rownames(effects) <- c('effect_mod', 'effect_nomod', 'difference')
      colnames(effects) <- c('estimate', 'st_err')
      effects$ci_lo <- effects$effect_mod - 1.96*st_err
      effects$ci_hi <- effects$effect_mod + 1.96*st_err
      effects$z <- 2 * (1 - pnorm(effects$estimate / effects$st_err))

      return(effects)}

    else {cat("Number of observations contained in the tmle fits and full data set differ. \nCheck that tmle_fit_0 and tmle_fit_1 together contain all observations in dat_full and vice versa.")}

  }

