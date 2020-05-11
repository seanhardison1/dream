#' Query NOAA Tides and Currents stations
#'
#' @param start Character vector start date for query ("YYYY-MM-DD").
#' @param end Character vector end date for query ("YYYY-MM-DD").
#' @param step Time step for data query. Default time step is 3 days ("3 days"), with a maximum time step of 30 days.
#' Larger time steps will result in faster downloads for longer time periods.
#' @param product Specify data product type. Defaults to "water_temperature".
#' See a complete listing of available products at \url{https://tidesandcurrents.noaa.gov/api/#products}. Note that all products are
#' not available at all stations.
#' @param station Station ID number. A complete listing of stations is available at \url{https://tidesandcurrents.noaa.gov/stations.html}.
#'
#' @note All times are GMT.
#'
#' @export
#'
#' @examples
#' query_tac(start = "1994-03-16",
#'           end ="1994-05-18",
#'           step = "30 days",
#'           product = "water_temperature",
#'           station = 8631044)

query_tac <- function(start, end, product, station, step = "3 days"){

  if (step == "1 day") step <- "1 days"
  step_num <- as.numeric(stringr::str_extract_all(step, "\\d+"))

  if (as.Date(end) - as.Date(start) < step_num){
    stop("Date range for query cannot be less than the length of the selected time step. Choose a smaller time step.")
  } else if (step_num > 30) {
    stop("Time step cannot be greater than 30 days. Choose a smaller time step.")
  }

  query_dates <-
    seq.Date(from = as.Date(start),
             to = as.Date(end),
             by = step)

  output <- NULL
  for (i in 1:length(query_dates)){

    start <- stringr::str_remove_all(as.character(query_dates[i]), "-")
    end <- stringr::str_remove_all(as.character(query_dates[i + 1]), "-")

    fname <- paste0("https://tidesandcurrents.noaa.gov/api/datagetter?product=",product,
                    "&application=NOS.COOPS.TAC.PHYSOCEAN&begin_date=", start,
                    "&end_date=",end,
                    "&station=",station,
                    "&time_zone=GMT&units=english&format=csv&application=UVA")

    int_dat <- suppressMessages(readr::read_csv(fname)) %>%
      dplyr::filter(complete.cases(.))

    assign('output', rbind(output, int_dat))

  }

  return(dplyr::distinct(output))

}




