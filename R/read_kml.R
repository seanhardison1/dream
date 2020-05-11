#' Read KML/KMZ files
#'
#' A function to read KML/KMZ files.
#'
#' @param filename Path to .kml or .kmz file
#' @param crs Coordinate reference system. Defaults to WGS84.
#'
#' @return Returns an sf object.
#'
#' @export
#' @import sf
#' @examples
#' fname <- system.file("extdata/MAB_poly.kml", package = "dream")
#' read_kml(fname)
#'


read_kml <- function(filename, cast = "LINESTRING",
                     crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") {

  if (stringr::str_detect(filename, "kmz")){
    SPP <- maptools::getKMLcoordinates(textConnection(system(paste("unzip -p",
                                                                   filename,sep = ""),
                                                             intern = TRUE)))
  } else {
    SPP <- maptools::getKMLcoordinates(paste(filename,sep = ""))
  }
    SPP <- data.frame(lat = SPP [[1]][,1],
                      lon = SPP [[1]][,2],
                      id = 1)
  sp::coordinates(SPP) <- ~lat + lon

  out <- SPP %>%
    as("sf") %>%
    dplyr::group_by(id) %>%
    dplyr::summarise(do_union=FALSE) %>%
    sf::st_cast(cast)

  sf::st_crs(out) <- crs

  return(out)
}


