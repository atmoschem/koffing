#' Get cookie from session
#'
#'
get_session_cookie <- function(url) {

  res <- httr::GET(url)

  httr::cookies(res)$value %>%
    purrr::set_names(httr::cookies(res)$name)

}

#' Log in the system
#'
#'
login_qualar <- function(url, login, password, cookie) {

  res <- httr::POST(
    url,
    body = list(
      cetesb_login = login,
      cetesb_password = password,
      enviar = "OK"
    ),
    encode = "form",
    httr::set_cookies(cookie)
  )
}

#' Search data
#'
#'
search_data <- function(url, station, parameter, start, end,
                        network, type, invalidData, cookie) {

  httr::POST(
    url,
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
    httr::set_cookies(cookie)
  )
}

#' Extract data
#'
#'
extract_data <- function(res) {

  httr::content(res) %>%
    rvest::html_table(fill = TRUE) %>%
    magrittr::extract2(2)

}


#' Clean data
#'
#' @export
clean_qualar_data <- function(df) {

  df <- janitor::remove_empty(df, "cols")

  col_names <- as.character(df[1,])

  df %>%
    magrittr::set_colnames(col_names) %>%
    dplyr::slice(-1) %>%
    dplyr::select(
      parameter = `Nome Parâmetro`,
      stationname = `Nome Estação`,
      date = Data,
      hour = Hora,
      mass_conc = `Média Horária`,
      mass_conc_movel = `Média Móvel`
    ) %>%
    dplyr::mutate(
      mass_conc = stringr::str_replace(mass_conc, ",", "."),
      mass_conc = as.numeric(mass_conc),
      time = as.POSIXct(x = paste(date, hour),
                         tz = "America/Sao_Paulo",
                         format = "%d/%m/%Y %H:%M"),
      date = lubridate::dmy(date),
      hour = stringr::str_sub(hour, start = 1, end = 2),
      hour = as.numeric(hour),
      dayofweek = lubridate::wday(date, label = TRUE),
      mass_conc = ifelse(abs(mass_conc) == 9999999, NA, mass_conc),
      parameter = stringr::str_replace_all(parameter, " [(].*", "")
    )
}

#' Safe clean data
#'
#'
safe_clean_data <- purrr::safely(clean_qualar_data)
