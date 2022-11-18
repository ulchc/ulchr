#' Upload the latest downloaded file to Google Sheets
#'
#' Uploads the latest file in the downloads folder of the specified type. A
#' string can be supplied to `name_contains` for a particular file.
#'
#' NOTE: Requires remotes::install_github("ulchc/vary")
#'
#' @param latest A string specifying a file type.
#' @param name_contains A string used to filter for a specific file name.
#' @param sheetname The spreadsheet name.
#' @param tabname The tab name.
#'
#' @return The input ss, as an instance of googlesheets4::sheets_id
#'
#' @importFrom readr read_tsv
#' @importFrom googlesheets4 gs4_create
#' @importFrom googlesheets4 sheet_rename
#' @export
#' @examples
#' upload_to_gsheets(latest = "tsv")
upload_to_gsheets <- function(
    latest = "tsv",
    name_contains = "",
    sheetname = "Export",
    tabname = "Export"
) {

  downloaded_tsv_files <- vary::files_matching(
    path = vary::get_downloads_folder(),
    name_contains = name_contains,
    file_type = latest
  )

  latest_tsv <- downloaded_tsv_files$path[1]

  data <- readr::read_tsv(latest_tsv)

  new_sheet <-
    googlesheets4::gs4_create(
      name = sheetname,
      sheets = data
    )

  googlesheets4::sheet_rename(new_sheet, 1, tabname)

  url <- paste0("https://docs.google.com/spreadsheets/d/", strsplit(new_sheet, "ID:"))

  browseURL(url)

  return(new_sheet)

}
