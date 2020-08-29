# `tmleplus`

A collection of functions to calculate additional effects and standard errors from from `tlverse`'s [`survtmle`](https://cran.r-project.org/web/packages/survtmle/survtmle.pdf) objects.

## Installation:

```devtools::install_github("hoffmakl/tmleplus")```

## Current capabilities:

### `surv_tidy()`

This function takes a fit object from the [`survtmle`](https://cran.r-project.org/web/packages/survtmle/survtmle.pdf) package and uses the influence curves to return a tidied data frame row with the difference, standard error, 95% confidence intervals, and p-value.

### `surv_contrast()`

This function takes two fit objects from the [`survtmle`](https://cran.r-project.org/web/packages/survtmle/survtmle.pdf) package and uses the influence curves to return a tidied data frame row with the difference, standard error, 95% confidence intervals, and p-value for one survtmle exposure estimate in comparison to a reference exposure estimate. This should be used when the exposure of interest is multinomial.

### `surv_eff_mod()`

This function takes two fit objects from the [`survtmle`](https://cran.r-project.org/web/packages/survtmle/survtmle.pdf) package and uses the influence curves to calculate the difference and standard errors between two marginal cumulative incidences, stratified by an effect modification variable.

### `surv_hists()`

This function takes a fit object from the [`survtmle`](https://cran.r-project.org/web/packages/survtmle/survtmle.pdf) package and returns four histograms to aid in model diagnostics: the predicted probability of treatment, predicted probabilities of censoring, predicted probabilities of the outcome, and influence curves.

