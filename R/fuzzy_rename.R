#' Rename columns with fuzzy matching
#'
#' Changes the names of all variables in `data` to the closest matches in
#' `match_with` using [vary::fuzzy_match()].
#'
#' @param data A data frame or tibble.
#' @param match_with A data frame, tibble, or vector of characters.
#' @seealso [vary::fuzzy_match()]
#' @return A renamed data frame or tibble.
#'
#' @import dplyr
#' @export
#' @examples
#'
#' library(dplyr)
#'
#' data <- tibble(
#'   ID     = "5.1.0",
#'   Code   = "222",
#'   Name   = "Book",
#'   Day    = "Friday",
#'   Month  = "APR",
#'   Amount = "19.00"
#' )
#'
#' messy_data <- tibble(
#'   `Amount $`           = "20.00",
#'   `Month (MMM) `       = "MAY",
#'   `Day of \n the week` = "Saturday",
#'   `Product\nName`      = "Notebook",
#'    Barcode             = "223",
#'   `ID #`               = "5.1.1"
#' )
#'
#' # underlying data is equivalent, but naming conventions are not
#' messy_data
#' data
#'
#' # no names are compatible between sources
#' tibble(names(data), names(messy_data))
#' # all names matched when using fuzzy_rename
#' tibble(names(data), names(fuzzy_rename(messy_data, names(data))))
#'
#' # automatically match, reorder, and combine
#' messy_data %>%
#'   fuzzy_rename(data) %>%
#'   select(names(data)) %>%
#'   rbind(data)
fuzzy_rename <- function(data, match_with) {
  if (typeof(match_with) != "character") {
    match_with <- names(match_with)
  }
  names(data) <- fuzzy_match(names(data), match_with)
  return(data)
}
