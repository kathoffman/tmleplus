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
#' @examples library(survtmle)
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

