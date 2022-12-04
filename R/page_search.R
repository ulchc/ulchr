#' Search through text for matching terms and return a tibble
#'
#' Returns sequences of characters at the `detail_level` specified, with each row
#' of the tibble containing the matching `term`. Special characters in `term`
#' are interpreted as fixed strings, however, "`|`" can be used to permit either-or
#' searching for multiple terms.
#'
#' @details This function was written for use with [pdftools::pdf_text()] or
#' [pdftools::pdf_ocr_text()], meaning that each item in the character vector is
#' assumed to be a page and is labeled as such on the returned tibble. If each
#' item is not representative of a page, the page column will only represent the
#' index of the original character vector.
#'
#' @param char_list A vector of characters (assumed to represent pages of text).
#' @param term A string to locate in the vector of characters.
#' @param detail_level One of: `c("page", "paragraph", "line")`.
#'
#' @return A tibble with columns `query`, `page`, `detail_level`, and `value`.
#' @import dplyr
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace_all
#' @seealso [flatten_pages()]
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
#' page_search(
#'   char_list = manual_text,
#'   term = "data",
#'   detail_level = "line"
#' )
#' }
page_search <- function(char_list, term, detail_level = c("page", "paragraph", "line")) {
  escape_term <- str_replace_all(term, "([^\\w|\\s|\\d])", "\\\\\\1")
  extract_term <- paste0(escape_term, ".*") %>% regex(ignore_case = T)

  if (detail_level == "page") {
    df_locate <-
      char_list %>%
      vary::flatten_pages(by = "paragraph") %>%
      mutate(found = ifelse(str_detect(value, extract_term), page, 0)) %>%
      with_groups(page, mutate, onpage = sum(found)) %>%
      filter(onpage != 0) %>%
      select(-onpage) %>%
      mutate(
        sep = if_else(lead(found) != found & lag(found) == 0, "\n* * * * *\n", ""),
        sep = if_else(lead(found) != found & found != 0, "\n* * * * *\n", sep),
        sep = if_else(is.na(sep) | sep == "", "\n\n", sep)
      ) %>%
      mutate(value = paste0(value, sep)) %>%
      group_by(page) %>%
      summarise(
        paragraph = paste0(paragraph, collapse = ", \n"),
        value = paste0(value, collapse = "")
      ) %>%
      mutate(query = paste0(gsub("|", '" or "', term, fixed = T), " (page match)")) %>%
      select(query, everything())
  } else if (detail_level == "paragraph") {
    df_locate <-
      char_list %>%
      vary::flatten_pages(by = "paragraph") %>%
      mutate(found = ifelse(str_detect(value, extract_term), page, 0)) %>%
      filter(found != 0) %>%
      select(-found) %>%
      mutate(sequence = as.numeric(str_extract(paragraph, "(?<=-).*"))) %>%
      mutate(sep = if_else(sequence + 1 != lead(sequence) | is.na(lead(sequence)), ". . .\n", "")) %>%
      mutate(value = paste0(value, "\n", sep, "\n")) %>%
      group_by(page) %>%
      summarise(
        paragraph = paste0(paragraph, collapse = ", "),
        value = paste0(value, collapse = "")
      ) %>%
      mutate(query = paste0(gsub("|", '" or "', term, fixed = T), " (paragraph match)")) %>%
      select(query, everything())
  } else if (detail_level == "line") {
    df_locate <-
      char_list %>%
      vary::flatten_pages(by = "line") %>%
      mutate(found = ifelse(str_detect(value, extract_term), page, 0)) %>%
      filter(found != 0) %>%
      select(-found) %>%
      mutate(sequence = as.numeric(str_extract(line, "(?<=-).*"))) %>%
      mutate(sep = if_else(sequence + 1 != lead(sequence) | is.na(lead(sequence)), ". . .\n", "")) %>%
      mutate(value = paste0(value, "\n", sep)) %>%
      group_by(page) %>%
      summarise(
        line = paste0(line, collapse = ", "),
        value = paste0(value, collapse = "")
      ) %>%
      mutate(query = paste0(gsub("|", '" or "', term, fixed = T), " (line match)")) %>%
      select(query, everything())
  }

  return(df_locate)
}
