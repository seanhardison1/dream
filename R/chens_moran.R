#' Global and local Moran's I
#'
#' Estimate global and local Moran's I (also known as LISA) using the methods proposed
#' in Chen 2013.
#'
#' @param df Data.frame with a unique observation (or sample) per row.
#' @param z The character name of the column holding the sample vector.
#' @param weight_func Spatial weight function forming the hypothetical spatial dependence
#' structure of observations. Currently limited to inverse distance ("inv") and negative
#' exponential ("nexp").
#' @param sample If TRUE, estimates are calculated for the spatial sample. If FALSE, estimates
#' are calculated for the spatial population. The choice of Moran's I for the spatial sample or
#' population depends on the scope of the study.
#'
#' @details `chens_moran` estimates global and local Moran's I using the methods proposed in
#' Chen 2013. Takes inspiration from the much more complete `Irescale` package
#' (Fuentes et al. 2019).
#'
#' @return A list object containing the global estimate for Moran's I (`global_estimate`) and
#' the input `data.frame` with new columns for the following:
#'
#' \item{`f`}{The matrix-vector product of the Real Spatial Weights Matrix and standardized vector `z`.
#' Represents the autocorrelation pattern of observations.}
#' \item{`f_star`}{The matrix-vector product of the Ideal Spatial Weights Matrix and the
#' standardized vector `z`. Represents the global autocorrelation estimate (i.e., the regression line
#' of f ~ z).}
#' \item{`f_residuals`}{The residuals of spatial autocorrelation (`f` - `f_star`). Useful for diagnosing
#' fit of spatial weights function.}
#' \item{`z`}{The standardized vector of observations.}
#' \item{`lisa`}{The Local Indicators of Spatial Association (LISA), or local Moran's I. Equivalent
#' to the diagonal of the Ideal Spatial Weights Matrix.}
#'
#'
#' @export
#'
#' @references
#'
#' Anselin, Luc. "The Moran scatterplot as an ESDA tool to assess local instability in spatial."
#' Spatial Analytical 4 (1996): 111.
#'
#' Chen, Yanguang. "New approaches for calculating Moran’s index of spatial autocorrelation."
#' PloS one 8.7 (2013).
#'
#' Ivan Fuentes, Thomas DeWitt, Thomas Ioerger and Michael Bishop (2019). Irescale:
#' Calculate and Rectify Moran's I. R package version 2.3.0.
#' https://CRAN.R-project.org/package=Irescale
#'
#' @examples
#'
#' library(dplyr)
#' library(ggplot2)
#'
#' # These data are rail distances between captial cities in China. Read them in and convert them
#' # to a matrix.
#' city_distances <- read.csv(system.file("extdata/city_distance_matrix.csv", package = "dream"))
#' dist_mat <- city_distances %>% dplyr::select(-X) %>% as.matrix()
#'
#' # pop_sizes are population sizes of Chinese capital cities in the year 2000.
#' pop_sizes <- read.csv(system.file("extdata/city_population_sizes.csv", package = "dream"))
#'
#' # Find global and local Moran's I for the sample using inverse distance weighting
#' output <- chens_moran(df = pop_sizes, z = "population", dist = dist_mat,
#' weight_func = "inv", sample = T)
#'
#' # Visualize Moran's scatterplot following Chen 2013. The slope of the regression line is global
#' # Moran's I, and the relationship between f and z represents the autocorrelation pattern among cities.
#'
#' ggplot(data = output$morans_scatter) +
#'  geom_point(aes(x = z, y = f)) +
#'  geom_line(aes(x = z, y = f_star))


chens_moran <- function(df, z, dist, weight_func = "nexp",
                        sample = T){

  # number of observations
  n <- length(df[[z]])

  if (!sample){
    # standard deviation for the population
    sd_pop <- sqrt( sum( (df[[z]] - mean(df[[z]])) ^ 2 ) / n )

    # standardize the vector
    z <- (df[[z]] - mean(df[[z]])) / sd_pop
  } else {
    # scale using standard deviation for the sample
    z <- scale(df[[z]])
  }

  # choose spatial weight function
  if (weight_func == "nexp"){
    W <- exp(-dist/mean(dist))
  } else if (weight_func == "inv"){
    W <- 1/dist
  }

  # Set diagonals to 0
  diag(W) <- 0

  # normalize to 1
  cSums = sum(W)
  W <- W/cSums

  # Get regression relationship
  f_star <- as.matrix(z) %*% as.matrix(t(z)) %*% as.matrix(W) %*% as.matrix(z)
  lisa <- diag(as.matrix(z) %*% as.matrix(t(z)) %*% as.matrix(W))
  f <- n * as.matrix(W) %*% as.matrix(z)

  ac_scatter <- tibble::tibble(f_star = as.numeric(f_star),
                       f = as.numeric(f),
                       f_residuals = f - f_star,
                       z = as.numeric(z),
                       lisa = as.numeric(lisa)) %>%
    dplyr::bind_cols(.,df)

  mod <- lm(f ~ z, ac_scatter)

  #global Moran's I (slope of the line)
  if (!sample){
    global_morans <- coef(mod)[2]
  } else {
    global_morans <- coef(mod)[2] * (n-1)/n
  }


  return(list(morans_scatter = ac_scatter,
              global_morans = global_morans))
}
