#' Title TODO THIS IS OLD FUNCTION FOR DEMO
#'
#' @param df Raw data frame from org at transactional level
#' @param org Three letter code used to check correct lookup table used
#' @param lookup_vector Named vector from global environment in app that provides old and used names
#'
#' @return Customer Behavior Table
#' @export
#'
#' @examples
create_customer_behavior_table <- function(df, org, lookup_vector) {

  # TODO: ADD if org empty, don't run, more safety checks, switch statement for orgs
  if (org != "ofs") {
    message("Wrong Org!!")
  }

  # TODO: ADD in PRIMARY customer based on most used AND most recent
  # (or check if issue if this changed during db date migration )

  df |>
    dplyr::rename(all_of(lookup_vector)) |>
    dplyr::select(
      customer_id,
      customer_account_name,
      customer_first_name,
      customer_full_name,
      customer_email_address,
      customer_mailing_address) |>
    dplyr::distinct() |>
    dplyr::mutate(customer_mailing_address = stringr::str_to_title(customer_mailing_address)) |>
    dplyr::arrange(customer_id) |>
    tidyr::nest(customer_contact_dict = c(customer_account_name, customer_first_name, customer_full_name, customer_email_address, customer_mailing_address))

}
