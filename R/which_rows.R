#' Return matching row indices
#'
#' For cleaning data with inconsistent formatting, but consistent identifying
#' characters somewhere within each row. For use in situations like:
#' * to detect the header row of an Excel sheet when it is always the first row without NA values
#' * to select rows by index when it is known that a valid row contains an identifying character or
#' string, but the column to search isn't consistent
#' * to normalize flattened data when more than one character or string must be used to
#' categorize a row value as a certain column attribute
#'
#' @param data Input data frame or tibble.
#' @param scan_cols The column indices that will have their row values combined and searched for matches
#' (\emph{see Details section}).
#' @param contain_strings A single string or character list.
#' @param all_strings If `TRUE`, return index where all strings present. If `FALSE`,
#'                    return the index of any row with at least one matching string.
#' @param case_sensitive If `FALSE`, pattern case is ignored. If `TRUE`, pattern case is considered.
#' @param lack_na If `TRUE`, do not return row indices which have `NA` values in any of the \code{scan_cols}.
#'                If `FALSE`, permit the return of indices with `NA` values.
#' @param flatten If `TRUE`, return an integer vector of each unique index. If `FALSE`,
#' return a named list of indices.
#' @details
#' Combines the row values of any number of
#' columns into a single string separated by a space (" ") and returns the indices
#' which pass the conditions of the arguments.
#'
#' @return An integer vectors or list of integer vectors.
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @export
#' @examples
#' library(tidyr)
#' library(dplyr)
#'
#' # Generate Data -------------------------------
#' set.seed(1)
#' data <- tidyr::table2
#' n_rows <- sample(nrow(data), 5)
#' data[n_rows, 1] <- table2[n_rows, 3]
#' data[n_rows, 3] <- table2[n_rows, 1]
#'
#' data # where type & country columns mismatched
#'
#' # EX. 1 ---------------------------------------
#' row_index <-
#'   which_rows(
#'     data,
#'     contain_strings = c("CASES", "2000"),
#'     all_strings = TRUE,
#'     case_sensitive = FALSE,
#'     flatten = TRUE
#'   )
#'
#' # NOTE: able to directly subset because flatten = TRUE
#'
#' # 3 rows returned which correspond with cases in 2000
#' data[row_index, ]
#' # 2 rows returned 1 missed with filter due to column misalignment
#' data %>% filter(type == "cases" & year == 2000)
#'
#' # EX. 2 ---------------------------------------
#' row_indices <-
#'   which_rows(
#'     data,
#'     contain_strings = c("1999", "population"),
#'     all_strings = FALSE,
#'     flatten = FALSE
#'   )
#'
#' # NOTE: list returned because flatten = FALSE
#'
#' # where "1999" occurs in string of row vals
#' data[row_indices$`contains string: '1999'`, ]
#' # where "population" occurs in string of row vals
#' data[row_indices$`contains string: 'population'`, ]
#'
#' # EX. 3 ---------------------------------------
#' row_index_alt <-
#'   which_rows(
#'     data,
#'     # which_rows combines cols for each row separated by " "
#'     contain_strings = c("1999 population"),
#'     all_strings = TRUE,
#'     flatten = TRUE
#'   )
#'
#' # cases where "1999" was found immediately to the left of "population"
#' data[row_index_alt, ]
which_rows <-
  function(data,
           scan_cols = 1:ncol(data),
           contain_strings = NULL,
           all_strings = TRUE,
           case_sensitive = TRUE,
           lack_na = TRUE,
           flatten = TRUE) {
    scan_df <- data[, scan_cols]
    string_rows <-
      scan_df %>%
      tidyr::unite("string_rows", names(scan_df), sep = " ", na.rm = T) %>%
      unlist()
    if (case_sensitive == FALSE & is.null(contain_strings) == FALSE) {
      contain_strings <- tolower(contain_strings)
      string_rows <- tolower(string_rows)
    }
    if (is.null(contain_strings) == FALSE) {
      contains_index <- map(.x = contain_strings, ~ stringr::str_which(string_rows, .x))
      names(contains_index) <- paste0("contains string: '", contain_strings, "'")
    } else {
      contains_index <- list(1:nrow(scan_df))
    }
    if (lack_na == T) {
      not_na <- which(rowSums(is.na(scan_df)) == 0)
      keep_indices <- purrr::map(.x = contains_index, ~ .x %in% not_na)
      contains_index <- purrr::map2(.x = contains_index, .y = keep_indices, ~ .x[.y])
    }
    if (length(contain_strings) == 1 | missing(contain_strings) == T) {
      if (flatten == TRUE) contains_index <- purrr::flatten_int(contains_index)
      return(contains_index)
    }
    if (all_strings == T) {
      flatten_index <- purrr::flatten_int(contains_index)
      all_strings_occur <-
        dplyr::tibble(index = flatten_index) %>%
        dplyr::group_by(index) %>%
        dplyr::summarise(occurs = n()) %>%
        dplyr::filter(occurs == length(contain_strings))
      contains_index <- list(all_strings_occur$index)
      names(contains_index) <- paste0("contains all strings: c('", paste0(contain_strings, collapse = "', '"), "')")
    }
    if (flatten == TRUE) contains_index <- sort(unique(purrr::flatten_int(contains_index)))
    return(contains_index)
  }
