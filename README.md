# `tmleplus`

A collection of functions to calculate additional effects and standard errors from various types of Targeted Maximum Likelihood Estimation (TMLE) objects.

Depending on the function, fit objects come from R packages [`tmle`](https://cran.r-project.org/web/packages/tmle/tmle.pdf), [`ltmle`](https://cran.r-project.org/web/packages/ltmle/ltmle.pdf), [`survtmle`](https://cran.r-project.org/web/packages/survtmle/survtmle.pdf), [`SuperLearner`](https://cran.r-project.org/web/packages/SuperLearner/SuperLearner.pdf), or [`sl3`](https://github.com/tlverse/sl3).

Functions are based off code originally written by Dr. Iván Díaz.

## Installation:

```devtools::install_github("hoffmakl/tmleplus")```

## Current capabilities:

### `surv_effect_mod()`

This function takes two fit objects from the `survtmle` package and uses the influence curves to calculate the difference and standard errors of the marginal cumulative incidence for an effect modifying variable.

