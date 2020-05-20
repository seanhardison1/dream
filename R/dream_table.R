#' Generate pretty LaTeX tables from glmmTMB and lme4 model summaries.
#'
#' Currently only works with crossed random-intercept models.
#'
#' @param model A mixed model fit with glmmTMB or lme4.
#' @param caption A caption to include with the table.
#' @param type Output type, defaulting to "latex". Also accepts "html".
#' @export
#'
#' @examples
#' \dontrun{
#' library(glmmTMB)
#' data(cbpp, package="lme4")
#' bovine <- glmmTMB(cbind(incidence, size-incidence) ~ period + (1|herd),
#'                  family=binomial, data=cbpp)
#' dream_table(model = bovine, caption = "Dream table for Binomial GLMM")
#' }

dream_table <- function(model, caption = NULL, type = "latex"){

  ### Model summaries
  fp <- broom.mixed::tidy(model) %>%
    dplyr::mutate(term = ifelse(is.na(std.error),
                                paste0("$\\sigma_{",group,"}$"), term),
                  effect = ifelse(effect == "fixed", "Fixed", "Random"))

  if (class(model) == "glmmTMB"){
    sp <- fp %>% dplyr::select(Effect = effect,
                               Parameter = term,
                               Estimate = estimate,
                               `Std. error` = std.error,
                               `$z$ value` = statistic,
                               `P-value` = p.value) %>%
      dplyr::mutate_at(dplyr::vars(Estimate:`P-value`), round, 3)

  } else if (class(model) == "glmerMod" | class(model) == "lmerMod") {
    sp <- fp %>% dplyr::select(Effect = effect,
                               Parameter = term,
                               Estimate = estimate,
                               `Std. error` = std.error,
                               `$z$ value` = statistic) %>%
      dplyr::mutate_at(dplyr::vars(Estimate:`$z$ value`), round, 3)
  }

  tble <- sp %>%
    dplyr::mutate_all(tidyr::replace_na, "") %>%
    knitr::kable(., type,
                              booktabs = T,
                              escape = F,
                              caption = caption,
                              longtable = T) %>%
    kableExtra::column_spec(1, bold = T) %>%
    kableExtra::collapse_rows(columns = 1, latex_hline = "major") %>%
    kableExtra::kable_styling(position = "center")

  return(tble)
}

