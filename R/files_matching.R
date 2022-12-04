#' Locate files in a directory
#'
#' Search a directory for a given file type and name, return
#' a tibble of matching file information sorted by modified date.
#'
#' @param path The path to the directory to locate files.
#' @param file_type A character vector indicating a file extension.
#' @param name_contains An optional character vector to filter for matching file names.
#' @param ignore.case If `TRUE`, `name_contains` case is ignored when filtering matching file names.
#' @return A three column tibble.
#' @import dplyr
#' @importFrom openxlsx write.xlsx
#' @export
#' @examples
#' # locate local OS downloads folder
#' downloads_dir <- vary::get_downloads_folder()
#' # write "Iris Report.xlsx" to downloads folder for example
#' openxlsx::write.xlsx(
#'   data.frame(iris),
#'   paste0(downloads_dir, "Iris Report.xlsx")
#' )
#'
#' # search downloads for Excel files with "report" included in the title
#' files_matching(downloads_dir, "xlsx", name_contains = "report")
#'
#' # search the R manual folder for pdf manuals (html also available)
#' r_manual_dir <- paste0(R.home("doc"), .Platform$file.sep, "manual")
#'
#' pdf_manuals <- files_matching(r_manual_dir, "pdf")
#' pdf_manuals
#'
#' # last modified (in this case, by the R installer)
#' latest_match <- pdf_manuals$path[1]
#' latest_match
files_matching <- function(path, file_type, name_contains = NULL, ignore.case = TRUE) {
  file_type <- gsub("[[:punct:]]", "", file_type)
  file_type <- paste0("*.", file_type, "$")
  name <-
    list.files(
      path = path,
      pattern = file_type,
      full.names = FALSE
    )
  path <-
    list.files(
      path = path,
      pattern = file_type,
      full.names = TRUE
    )
  last_modified <- file.mtime(path)
  matching_df <- dplyr::tibble(name, path, last_modified)
  matching_df <- dplyr::arrange(matching_df, desc(last_modified))
  if (!missing(name_contains)) {
    matching_df <-
      dplyr::filter(
        matching_df,
        grepl(
          name_contains,
          name,
          ignore.case
        )
      )
  }
  return(matching_df)
}
