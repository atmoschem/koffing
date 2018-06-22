#' Scraper CETESB
#'
#' @param station A numeric value indicating the station id from
#' where you wish to get the data.
#' See [koffing::cetesb_station_ids].
#' @param parameter A numeric value indicating the CETESB parameter
#' id. See [koffing::cetesb_station_ids].
#' @param start A string in the format "dd/mm/aaaa" representing
#' the initial day for the data selection.
#' @param end A string in the format "dd/mm/aaaa" representing  the
#' final day for the data selection.
#' @param type Type of data: "P" for hourly mean or "M" for moving
#' average.
#' @param login A string with your login on Qualar system.
#' @param password A string with your passoword on Qualar system.
#' @param invalidData If TRUE, the system will return rows with
#' invalid data.
#' @param network Network type: "A" for automatic or "M" for manual.
#' @param cleanData Should the data be cleaned before returned? The
#' default is TRUE. Set FALSE to return the data as extracted from Qualar.
#'
#' @return A tibble with the data returned by the Qualar system.
#' @importFrom magrittr "%>%" set_colnames extract2
#' @importFrom httr GET cookies POST set_cookies content
#' @importFrom purrr set_names
#' @importFrom rvest html_table
#' @importFrom janitor remove_empty
#' @importFrom dplyr slice
#' @examples
#' \dontrun{
#'
#' # Ozone for 'Dom Pedro II' station from 01/01/2018 to 31/01/2018.
#'
#' scraper_cetesb(station = 72, parameter = 63,
#'                start = "01/01/2018", end = "31/01/2018",
#'                login = "my_login", password = "my_password")
#' }
#' @export

scraper_cetesb <- function(station, parameter, start, end, login, password,
                           type = "P", invalidData = "on", network = "A",
                           cleanData = TRUE) {

  cookie <- get_session_cookie(
    url = "http://qualar.cetesb.sp.gov.br/qualar/home.do"
  )

  res <- login_qualar(
    url = "http://qualar.cetesb.sp.gov.br/qualar/autenticador",
    login = login,
    password = password,
    cookie = cookie
  )

  res <- search_data(
    url = "http://qualar.cetesb.sp.gov.br/qualar/exportaDados.do",
    station = station,
    parameter = parameter,
    start = start,
    end = end,
    network = network,
    type = type,
    invalidData = invalidData,
    cookie = cookie
  )

  data <- extract_data(res)

  data

  if(cleanData == TRUE) {

    clean_data <- safe_clean_data(data)

    if(is.null(clean_data$result)) {
      warning(
        "The clean data function didn't work. The qualar system may have changed. The raw data was returned."
      )
      data
    } else {
      clean_data$result
    }

  } else {
    data
  }

}

