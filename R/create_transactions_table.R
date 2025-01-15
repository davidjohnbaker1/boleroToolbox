#' Create Transactions Table
#'
#' @param df Raw data from org at transaction level
#' @param org Three letter code used to check correct lookup table used
#' @param lookup_vector Named vector from global environment in app that provides old and used names
#'
#' @return Transactions table
#' @export
#'
#' @examples
create_transactions_table <- function(df, org, lookup_vector) {

  df |>
    dplyr::rename(all_of(lookup_vector)) |>
    dplyr::select(starts_with("transaction"), customer_id, event_datetime, event_ticketable_name) |>
    # TODO: Performance Bottleneck
    # TODO: Could consider just using Time as the key for this, seems like bad idea
    dplyr::mutate(
      event_datetime = lubridate::mdy_hm(event_datetime),
      event_id = paste(event_ticketable_name, event_datetime)
    )

}
