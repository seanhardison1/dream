#' Assess model fit for multiple models through DHARMa residual simulation
#'
#' This function applies DHARMa validation tests to a user-specified list of models. Tests
#' look for agreement between simulated and empirical residuals; assessing models for dispersion,
#' zero-inflation, anomalous prevalence of outliers, and deviations from expected distributions.
#'
#' @param mod_set A list of model objects.
#' @param n Number of DHARMa simulations to generate from each model.
#' @param p.adjust.method Defaults to \code{none}, but takes any correction method specified with \code{\link{p.adjust}}.
#'
#' @details The philosophy behind this approach is that models being compared using information criteria
#' should meet modeling assumptions regardless of complexity, otherwise model selection
#' may provide a parsimonious, poorly fitting model (Mazerolle 2019). This method should not
#' be a replacement for visual inspection of model fit, but may be useful in an
#' exploratory context, especially with more complicated hierarchical models. To remain conservative,
#' resultant P values from DHARMa tests are not adjusted for multiple comparison by default, although
#' P adjustment methods may be specified using \code{p.adjust.method}.
#'
#' @return Returns a data.frame containing model formulas for each model and P values for
#' each test applied.
#'
#' @export validate
#'
#' @references
#'
#' Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests of significance. Biometrika, 75, 800–803. doi: 10.2307/2336325.
#'
#' Hartig, Florian. 2017. “DHARMa: Residual Diagnostics for Hierarchical (Multi-Level/Mixed) Regression Models.” R Package Version 0.1 5.
#'
#' Mazerolle, Marc J. 2019. AICcmodavg: Model Selection and Multimodel Inference Based on (Q)AIC(c). https://cran.r-project.org/package=AICcmodavg.
#'
#' @examples
#'
#' library(glmmTMB)
#' mod_list <- list()
#' mod_list[[1]] <- glmmTMB(count ~ mined + (1|site), zi=~mined, family=poisson, data=Salamanders)
#' mod_list[[2]] <- glmmTMB(count ~ mined + cover + (1|site), zi=~mined, family=poisson, data=Salamanders)
#' mod_list[[3]] <- glmmTMB(count ~ mined + cover + spp + (1|site), zi=~mined, family=poisson, data=Salamanders)
#' dream::validate(mod_list)
#'
validate <- function(mod_set, n = 1000, p.adjust.method = "none"){
  # Do model validation
  df <- do.call(rbind,
                (lapply(mod_set, loopdy, n = n)))

  mod_form <- df %>%
    dplyr::select(response, predictors) %>%
    dplyr::mutate(id = 1:length(mod_set))
  ps <- df %>%
    dplyr::select(-response, -predictors) %>%
    dplyr::mutate(id = 1:length(mod_set)) %>%
    tidyr::gather(Var, Value, -id) %>%
    dplyr::mutate(Value = stats::p.adjust(Value, method = p.adjust.method)) %>%
    tidyr::spread(Var, Value) %>%
    dplyr::left_join(.,mod_form, by = "id") %>%
    dplyr::select(-id)
  return(ps)
}

loopdy <- function(mod_set,n = n){
  scaled_sims <- DHARMa::simulateResiduals(fittedModel = mod_set, n = n)
  res_tests <- hush(DHARMa::testResiduals(scaled_sims, plot = F))
  zinf <- DHARMa::testZeroInflation(scaled_sims, plot = F)

  out <- data.frame(response = as.character(formula(mod_set))[2],
                    predictors =  as.character(formula(mod_set))[3],
                    uniformity_p = res_tests[[1]]$p.value,
                    dispersion_p = res_tests[[2]]$p.value,
                    outliers_p = res_tests[[3]]$p.value,
                    zeroinf_p = zinf$p.value)
  return(out)
}

hush=function(code){
  if(.Platform$OS.type == "unix") {
    sink("/dev/null")
  } else {
    sink("NUL")
  }
  tmp = code
  sink()
  return(tmp)
}
