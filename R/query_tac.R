#' Query NOAA Tides and Currents stations
#'
#' @param start Character vector start date for query ("YYYY-MM-DD").
#' @param end Character vector end date for query ("YYYY-MM-DD").
#' @param step Time step for data query. Default time step is 3 days ("3 days"), with a maximum time step of 30 days.
#' Larger time steps will result in faster downloads for longer time periods.
#' @param product Specify data product type. Defaults to "water_temperature".
#' See a complete listing of available products at \url{https://tidesandcurrents.noaa.gov/api/#products}. Note that all products are
#' not available at all stations. All data are returned in metric units.
#' @param station Station ID number. A complete listing of stations is available at \url{https://tidesandcurrents.noaa.gov/stations.html}.
#' @param datum Queries regarding water level require a datum. If none is selected, then datum defaults to "MLLW," or Mean-Lower Low Water. Example datum selections are available to view at \url{https://tidesandcurrents.noaa.gov/datums.html?id=8722607}.
#' @param tz Data product time zone. Defaults to GMT ("gmt"). Other options include Local Standard Time ("lst") and Local Standard/Local Daylight Time ("lst_ldt").
#'
#'
#' @export
#'
#' @examples
#' query_tac(start = "1994-03-16",
#'           end ="1994-05-18",
#'           step = "30 days",
#'           product = "water_temperature",
#'           station = 8631044)

query_tac <- function(start, end, product,
                      station, step = "3 days",
                      datum = "MLLW",
                      tz = "gmt"){

  if (step == "1 day") step <- "1 days"
  step_num <- as.numeric(stringr::str_extract_all(step, "\\d+"))

  user_end <- end
  if (as.Date(user_end) - as.Date(start) < step_num){
    stop("Date range for query cannot be less than the length of the selected time step. Choose a smaller time step.")
  } else if (step_num > 30) {
    stop("Time step cannot be greater than 30 days. Choose a smaller time step.")
  }

  query_dates <-
    seq.Date(from = as.Date(start),
             to = as.Date(user_end),
             by = step)

  output <- NULL
  for (i in 1:length(query_dates)){

    start <- stringr::str_remove_all(as.character(query_dates[i]), "-")
    if (i != length(query_dates)){
      end <- stringr::str_remove_all(as.character(query_dates[i + 1]), "-")
    } else {
      end <- stringr::str_remove_all(as.character(user_end), "-")
    }


    if (product %in% c("water_temperature", "conductivity", "salinity",
                       "currents", "currents_predictions")){
      server <- "NOS.COOPS.TAC.PHYSOCEAN"
    } else if (product %in% c("water_level", "one_minute_water_level",
                              "predictions", "datums",
                              "monthly_mean", "daily_mean",
                              "high_low", "hourly_height")){
      server <- "NOS.COOPS.TAC.WL"
    } else if (product %in% c("air_temperature", "wind", "air_pressure", "air_gap",
                              "visibility", "humidity")){
      server <- "NOS.COOPS.TAC.MET"
    } else {
      stop("Product name misspelled or not available.")
    }

    fname <- paste0("https://tidesandcurrents.noaa.gov/api/datagetter?product=",product,
                    "&application=",server,"&begin_date=", start,
                    "&end_date=",end,
                    "&station=",station,
                    "&time_zone=",tz,
                    "&units=metric&format=csv&application=UVA")

    fname <- ifelse(server == "NOS.COOPS.TAC.WL", stringr::str_glue(fname, paste0("&datum=",datum)), fname)

    int_dat <- suppressMessages(readr::read_csv(fname)) %>%
      dplyr::filter(complete.cases(.))

    assign('output', rbind(output, int_dat))

  }

  return(dplyr::distinct(output))

}

