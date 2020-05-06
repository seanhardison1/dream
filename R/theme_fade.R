#' A ggplot2 theme
#'
#' @export
#'
#' @examples
#'
#' library(ggplot2)
#' ggplot(iris) +
#'    geom_point(aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'    theme_fade()

theme_fade <- function(...) {
  theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = "black",
                                  fill = NA, size = 0.2),
      legend.key = element_blank(),
      axis.title = element_text(size = 10))
}
