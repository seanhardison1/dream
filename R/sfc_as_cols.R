#' Convenience function to convert sfc to lat/lon columns within a data.frame.
#'
#' @param x An \code{sf} object
#' @param names Names for longitude and latitude columns
#'
#' @details Originally written by Josh London here -> https://github.com/r-spatial/sf/issues/231
#'
#' @export
sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}
