#' Generate pretty LaTeX tables from glmmTMB summaries
#'
#' @param model A glmmTMB model object.
#' @param caption A caption to include with the table.
#'
#'
#' @examples
#' data(cbpp, package="lme4")
#' bovine <- glmmTMB(cbind(incidence, size-incidence) ~ period + (1|herd),
#'                  family=binomial, data=cbpp)
#' dream_table(model = bovine, caption = "Dream table for Binomial GLMM")

dream_table <- function(model, caption){
  ### Model summaries
  broom.mixed::tidy(model) %>%
    dplyr::mutate(term = ifelse(is.na(std.error),
                         paste0("$\\sigma_{",group,"}$"), term),
           effect = ifelse(effect == "fixed", "Fixed", "Random")) %>%
    dplyr::select(Effect = effect,
                  Parameter = term,
                  Estimate = estimate,
                  `Std. error` = std.error,
                  `$z$ value` = statistic,
                  `P-value` = p.value) %>%
    dplyr::mutate_at(dplyr::vars(Estimate:`P-value`), round, 3) %>%
    dplyr::mutate_all(tidyr::replace_na, "") %>%

    knitr::kable(., "latex",
          booktabs = T,
          escape = F,
          caption = caption,
          longtable = T) %>%
    kableExtra::column_spec(1, bold = T) %>%
    kableExtra::collapse_rows(columns = 1, latex_hline = "major") %>%
    kableExtra::kable_styling(position = "center")
}


