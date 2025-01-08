#' toy_calculate_mrr
#'
#' @param n_customers The Number of Customers
#' @param product_price How Much You Might Charge
#'
#' @return ans
#' @export
#'
#' @examples
#' toy_calculate_mrr(n_customers = 10, product_price = 300)
toy_calculate_mrr <- function(n_customers, product_price) {
  ans <- n_customers * product_price
  return(ans)
}
