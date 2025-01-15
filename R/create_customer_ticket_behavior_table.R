#' Create Customer Ticket Behavior Table
#'
#' @param transaction_df Result of `boleroToolbox::create_transactions_table()`
#' @param org Three letter code used to check correct lookup table used
#' @param recent_patron_max Maximum days allowed for patron to be considered recent (90 default)
#' @param current_patron_max Maximum days allowed for current patron (270 default)
#' @param expiring_patron_max Maximum days allowed for expiring patron (1 Year default)
#' @param lapsed_patron_max Maximum days allowed for lapsed patron (3 Years default)
#'
#' @return Table of all transactions with ticket behavior metrics
#' @export
#'
#' @examples
create_customer_ticket_behavior_table <- function(transaction_df,
                                                  org,
                                                  recent_patron_max = 90,
                                                  current_patron_max = 270,
                                                  expiring_patron_max = 365,
                                                  lapsed_patron_max = 1095) {

  # Start
  transaction_df |>
    dplyr::select(customer_id,
                  transaction_date,
                  transaction_id,
                  transaction_order_amount) |>
    dplyr::arrange(customer_id, transaction_date) |>
    dplyr::group_by(customer_id) |>
    dplyr::distinct() |>
    dplyr::mutate(
      # Constants
      today = lubridate::today(),
      # Number transactions chronologically
      transaction_number = dplyr::row_number(),
      # Calculate days since last purchase
      days_since_previous_transaction = as.numeric(difftime(
        transaction_date,
        dplyr::lag(transaction_date, default = first(transaction_date)),
        units = "days"
      )),
      days_since_last_transaction = as.numeric(difftime(today, max(transaction_date), units = "days")),
      customer_cumulative_spend = base::cumsum(transaction_order_amount),
      # Key Calculation - Last Transaction Status
      customer_current_member_activity_status = dplyr::case_when(
        days_since_last_transaction >= 0 &
          days_since_last_transaction < recent_patron_max ~ "recent_patron",
        days_since_last_transaction >= recent_patron_max &
          days_since_last_transaction < current_patron_max ~ "current_patron",
        days_since_last_transaction >= current_patron_max &
          days_since_last_transaction < expiring_patron_max ~ "expiring_patron",
        days_since_last_transaction >= expiring_patron_max &
          days_since_last_transaction < lapsed_patron_max ~ "lapsed_patron",
        days_since_last_transaction >= lapsed_patron_max  ~ "dormant_patron",
        .default = "FIX"
      ),
      # Key Calculation - Last Transaction Status - LongHaul
      customer_historical_longhaul_behavior = dplyr::case_when(
        transaction_number == 1 ~ "first_time_attendee",
        days_since_previous_transaction > 0 &
          days_since_previous_transaction < expiring_patron_max ~ "multi_repeat_buyer",
        days_since_previous_transaction >= expiring_patron_max &
          days_since_previous_transaction < lapsed_patron_max ~ "lapsed_buyer",
        days_since_previous_transaction >= lapsed_patron_max ~ "dormant_buyer",
        .default = "FIX"
      )
    ) |>
    dplyr::ungroup()

}
