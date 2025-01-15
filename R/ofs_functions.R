#' OFS Clean Transactions Table
#'
#' @param ofs_transaction_table  Cleaned transactions table for OFS
#'
#' @return Table with internal variables standardized
#' @export
#'
#' @examples
ofs_clean_transaction_order_type <- function(ofs_transaction_table) {

  ofs_transaction_table |>
    dplyr::mutate(transaction_order_type = dplyr::case_when(
      transaction_order_type == "Membership" ~ "product_membership",
      transaction_order_type == "Tickets" ~ "product_tickets",
      transaction_order_type == "Tickets; Membership" ~ "product_membership_ticket",
      transaction_order_type == "Tickets; Subscription" ~ "product_subscription_ticket",
      .default = "FIX"

    ))

}
