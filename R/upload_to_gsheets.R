#' Upload the latest downloaded file to Google Sheets
#'
#' Uploads the latest file in the downloads folder of the specified type. A
#' string can be supplied to `name_contains` for a particular file.
#'
#' @param latest A string specifying a file type (csv or tsv).
#' @param name_contains A string used to filter for a specific file name.
#' @param sheetname The spreadsheet name.
#' @param tabname The tab name.
#'
#' @return The input ss, as an instance of googlesheets4::sheets_id
#'
#' @importFrom readr read_tsv
#' @importFrom readr read_csv
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

  downloaded_files <- vary::files_matching(
    path = vary::get_downloads_folder(),
    name_contains = name_contains,
    file_type = latest
  )

  latest_file <- downloaded_files$path[1]

  if (latest == "tsv") {
    data <- readr::read_tsv(latest_file)
  } else if (latest == "csv") {
    data <- readr::read_csv(latest_file)
  } else {
    stop("Only tsv and csv files are compatible with this function")
  }

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
