#' Structure pages of text into tabular form
#'
#' Converts a character vector into a tibble with each row representing
#' a page, paragraph, or line of text. A string other than `page`, `paragraph`, or
#' `line` can be supplied to the `by` argument to separate text into rows using
#' that string.
#'
#' @details This function was written for use with [pdftools::pdf_text()] or
#' [pdftools::pdf_ocr_text()], meaning that each item in the character vector is
#' assumed to be a page and is labeled as such on the returned tibble. If each
#' item is not representative of a page, the page column will only represent the
#' index of the original character vector.
#'
#' @param page_list A vector of characters (assumed to represent pages of text).
#' @param by One of: `c("page", "paragraph", "line")` or a custom string to split text into rows.
#'
#' @return A tibble with columns `page`, `index`, and `value`.
#' @import dplyr
#' @import purrr
#' @import stringr
#' @importFrom pdftools pdf_ocr_text
#' @importFrom pdftools pdf_text
#' @importFrom tidyr separate_rows
#' @export
#' @examples
#' \dontrun{
#' # return the R manual folder path
#' r_manual_dir <- paste0(R.home("doc"), .Platform$file.sep, "manual")
#'
#' # return the first pdf manual in the directory
#' latest_match <- files_matching(r_manual_dir, "pdf")$path[1]
#'
#' manual_text <- pdftools::pdf_text(latest_match)
#'
#' flatten_pages(manual_text, by = "page")
#' flatten_pages(manual_text, by = "line")
#' flatten_pages(manual_text, by = "paragraph")
#' flatten_pages(manual_text, by = ":")
#' }
flatten_pages <- function (page_list, by = c("page", "paragraph", "line")) {
  page_df <-
    pmap_dfr(list(page_list), as_tibble) %>%
    mutate(`page` = row_number()) %>%
    relocate(`page`, .before = value)
  if (by == "page") {
    return(page_df)
  } else if (by == "paragraph") {
    page_df <-
      page_df %>%
      separate_rows(value, sep = fixed("\n\n"))
  } else if (by == "line") {
    page_df <-
      page_df %>%
      separate_rows(value, sep = fixed("\n"))
  } else {
    page_df <-
      page_df %>%
      separate_rows(value, sep = fixed(by))
  }
  start_marks <- map2_dbl(.x = pull(page_df, `page`), .y = lag(pull(page_df, `page`)), ~ifelse(.x != .y | is.na(.y), 1, 0))
  longest_seq <- paste0(start_marks, collapse = "") %>%
    str_split("1") %>%
    unlist() %>%
    nchar() %>%
    max()
  page_by <- start_marks
  for (i in 1:longest_seq) {page_by <- ifelse(lag(page_by) != 0 & page_by == 0,  i + 1, page_by)}
  page_df$page_by <- paste0(page_df$`page`, "-", page_by)
  if (by == "paragraph") {
    page_df <- page_df %>% select(`page`, paragraph = page_by, value)
  } else if (by == "line") {
    page_df <- page_df %>% select(page, line = page_by, value)
  } else {
    page_df <- page_df %>% select(`page`, page_by, value)
    names(page_df) <- c("page", paste0(by, "_index"), "value")
  }
  return(page_df)
}


