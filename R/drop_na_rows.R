#' Drop NA rows
#'
#' Returns a tibble or data frame without rows containing only NA values. If
#' `percent_incomplete` is supplied, any row which has a total percentage of NA
#' values greater than or equal to `percent_incomplete` will be dropped.
#'
#' @param df A data frame or tibble.
#' @param percent_incomplete A numeric value.
#' @return A data frame or tibble.
#' @export
#' @seealso [drop_na_cols()]
#' @examples
#' library(tidyr)
#'
#' sample_data <-
#'   tibble(rbind(
#'     head(iris, 5),
#'     rep(NA, 5),
#'     c(1, rep(NA, 4)))
#'   )
#'
#' sample_data
#'
#' drop_na_rows(sample_data)
#' drop_na_rows(sample_data, percent_incomplete = 75)
drop_na_rows <- function(df, percent_incomplete = 100) {
  return(df[(1 - rowSums(is.na(df))/ncol(df))*100 > (100 - percent_incomplete), ])
}
