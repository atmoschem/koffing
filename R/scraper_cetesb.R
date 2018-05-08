#' Scraper CETESB
#'
#' @param station A numeric value indicating the station id from
#' where you wish to get the data.
#' See [Rpollution::cetesb_station_ids].
#' @param parameter A numeric value indicating the CETESB parameter
#' id. See [Rpollution::cetesb_station_ids].
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
#'
#' @return A tibble with the data returned by the Qualar system.
#'
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

scraper_cetesb <- function(station, parameter, start,
                           end, type = "P", login,
                           password, invalidData = "on",
                           network = "A") {

  # Get cookie

  res <- httr::GET("http://qualar.cetesb.sp.gov.br/qualar/home.do")

  my_cookie <- httr::cookies(res)$value %>%
    purrr::set_names(httr::cookies(res)$name)

  # Login

  url_login <- "http://qualar.cetesb.sp.gov.br/qualar/autenticador"

  res <- httr::POST(
    url_login,
    body = list(
      cetesb_login = login,
      cetesb_password = password,
      enviar = "OK"
    ),
    encode = "form",
    httr::set_cookies(my_cookie)
  )

  # Search data on system

  url_data <- "http://qualar.cetesb.sp.gov.br/qualar/exportaDados.do"

  res <- httr::POST(
    url_data,
    query = list(method = "pesquisar"),
    body = list(
      irede = network,
      dataInicialStr  = start,
      dataFinalStr = end,
      cDadosInvalidos = invalidData,
      iTipoDado = type,
      estacaoVO.nestcaMonto = station,
      parametroVO.nparmt = parameter
    ),
    encode = "form",
    httr::set_cookies(my_cookie)
  )

  # Extract data form html and clean the dataset

  cetesb_data <- httr::content(res) %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(2) %>%
    janitor::remove_empty_cols()

  col_names <- as.character(cetesb_data[1,])

  cetesb_data %>%
    magrittr::set_colnames(col_names) %>%
    dplyr::slice(-1)

}
