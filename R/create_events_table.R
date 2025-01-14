#' Create Events Table
#'
#' @param df Raw data from org at transaction level
#' @param org Three letter code used to check correct lookup table used
#' @param lookup_vector Named vector from global environment in app that provides old and used names
#'
#' @return Events Table
#' @export
#'
#' @examples
create_events_table <- function(df, org, lookup_vector) {

  # TODO: Saftey Check for Org?

  df |>
    dplyr::rename(all_of(lookup_vector)) |>
    dplyr::select(event_datetime, event_ticketable_name, event_type) |>
    dplyr::distinct() |>
    dplyr::mutate(event_datetime = lubridate::mdy_hm(event_datetime)) |>
    # PRIMARY KEY
    dplyr::mutate(event_id = paste(event_ticketable_name, event_datetime)) |>
    dplyr::mutate(
      event_year = lubridate::year(event_datetime),
      event_month = lubridate::month(event_datetime),
      event_day = lubridate::day(event_datetime),
      event_weekday = lubridate::wday(event_datetime, abbr = FALSE),
      event_hour = lubridate::hour(event_datetime),
      event_minute = lubridate::minute(event_datetime),
      event_showtime = dplyr::case_when(
        event_hour <= 11 ~ "Morning",
        event_hour <= 16 ~ "Matinee",
        event_hour <= 20 ~ "Evening",
        event_hour <= 23 ~ "Late",
        .default = "Fix"
      )
    )

}
