#' toy_calculate_mrr
#'
#' @param n_customers
#' @param product_price
#'
#' @return mrr
#' @export
#'
#' @examples
#' toy_calculate_mrr(n_customers = 10, product_price = 300)
toy_calculate_mrr <- function(n_customers, product_price) {
  n_customers * product_price
}
