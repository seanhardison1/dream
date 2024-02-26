#'Convert rasters to tibbles.
#'
#'@param r Raster object to be converted.
#'@param var_name Column name for fill variable. Defaults to `fill_var`.
#'@param xy_name Column names for x and y variables (in that order). Defaults to `longitude` and `latitude`.
#'
#' @return A `tibble` with three columns: a fill variable, longitude, and latitude.
#'
#' @export
#'
#' @examples
#' library(raster)
#' f <- system.file("external/test.grd", package="raster")
#' r <- raster(f)
#' tib <- rst_to_tib(r)
#' tib
#'
#' @import dplyr

rst_to_tib <- function(r, var_name = "fill_var",
                      xy_name = c("longitude","latitude")){
  out <-
    r %>%
    as("SpatialPixelsDataFrame") %>%
    as_tibble()
  names(out)[1] <- var_name
  names(out)[2:3] <- xy_name
  return(out)
}


