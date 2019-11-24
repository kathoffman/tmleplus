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
#' surv_eff_mod(tmle_fit_0 = fit_noeff, tmle_fit_1 = fit_eff)

surv_eff_mod <- function(tmle_fit_1, tmle_fit_0){

    # get relevant N's for later calculations
    n_1 <- nrow(tmle_fit_1$ic)
    n_0 <- nrow(tmle_fit_0$ic)
    N   <- n_0 + n_1

    # obtain differences in IC for treated vs untreated in each effect mod strata
    ic_1 <- tmle_fit_1$ic[, 2] - tmle_fit_1$ic[, 1]
    ic_0 <- tmle_fit_0$ic[, 2] - tmle_fit_0$ic[, 1]

    # normalize ICs to proportion of observations with and without effect mod var
    ic_1 <- ic_1 / (n_1 / N)
    ic_0 <- ic_0 / (n_0 / N)

    # add in number of observations from the opposite strata as zero values
    # 0s are added in opposite sides of vectors so we can calculate diff in ICs later
    ic_1 <- c(ic_1, rep(0, n_0))
    ic_0 <- c(rep(0, n_1), ic_0)

    # calculate the standard errors for each IC and their difference
    se_1 <- sd(ic_1) / sqrt(N)
    se_0 <- sd(ic_0) / sqrt(N)
    se_effmod <- sd(ic_1 - ic_0) / sqrt(N)

    # get the estimates and difference
    effect_1 <- diff(tmle_fit_1$est[, 1])
    effect_0 <- diff(tmle_fit_0$est[, 1])
    effmod <- effect_0 - effect_1

    # combine into a data frame and calculate 95% CIs and P-values
    effects <- data.frame(cbind(c(effect_1, effect_0, effmod),
                                c(se_1, se_0, se_effmod)))
    rownames(effects) <- c('effect_mod', 'effect_nomod', 'difference')
    colnames(effects) <- c('estimate', 'st_err')
    effects$ci_lo <- effects$estimate - 1.96 * effects$st_err
    effects$ci_hi <- effects$estimate + 1.96 * effects$st_err
    effects$p_val <- 2 * (1 - pnorm(abs(effects$estimate / effects$st_err)))

    return(effects)

    }




