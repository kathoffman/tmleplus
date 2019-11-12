# `tmleplus`

A collection of functions produced with the help of Dr. Iván Díaz to calculate additional effects and standard errors from various types of Targeted Maximum Likelihood Estimation (TMLE) objects.

Depending on the function, fit objects come from R packages [`tmle`](https://cran.r-project.org/web/packages/tmle/tmle.pdf), [`ltmle`](https://cran.r-project.org/web/packages/ltmle/ltmle.pdf), [`survtmle`](https://cran.r-project.org/web/packages/survtmle/survtmle.pdf), [`SuperLearner`](https://cran.r-project.org/web/packages/SuperLearner/SuperLearner.pdf), or [`sl3`](https://github.com/tlverse/sl3).

## Installation:

```devtools::install_github("hoffmakl/tmleplus")```

## Current capabilities:

### `surv_eff_mod()`

This function takes two fit objects from the `survtmle` package and calculates the difference and standard errors of the cumulative hazard (or mean) incidence for an effect modifying variable using influence curves.

