#' Factor to numeric
#'
#' Easily convert a factor to numeric without losing information.
#'
#' @param x A factor.
#'
#' @return A numeric.
#'
#' @export
#'
#' @examples
#'
#'library(dplyr)
#'set.seed(1)
#'num <- rnorm(10)
#'
#'
#'# Without fct_to_numeric
#'data.frame(fac = factor(num)) %>%
#'  mutate(bad_num = as.numeric(fac))
#'
#'# With fct_to_numeric
#'data.frame(fac = factor(num)) %>%
  #'  mutate(bad_num = fct_to_num(fac))

fct_to_num <- function(x){
  as.numeric(levels(x))[x]
}
