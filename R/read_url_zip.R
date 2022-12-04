#' Load data from online zipped folders
#'
#' Downloads the zip folder pointed to by `url`, unzips it to a temporary
#' location, reads the first csv into R as a tibble, then deletes the downloaded
#' zip folder and the temporary files.
#'
#' `file_type`, `file_name`, and `file_index` can be changed from the defaults
#' to read a file that is not the first csv. If the file specified is not a "csv",
#' "tsv", "xlsx", "xls", "pdf", or "txt", a function to read the file with
#' must be specified using the `reader` parameter.
#'
#' @param url The download link to a zipped folder online (ending with .zip).
#' @param file_type A character vector indicating a file extension.
#' @param file_name An optional string to specify which file to load using the file name.
#' @param file_index An optional index number to specify which file to load when `file_name` isn't specified.
#' @param reader A string indicating which function to use when reading the file into R.
#' @param check_name_for_type Boolean option to prevent detecting `file_type` from
#' the last 5 characters of `file_name`. Use if the last few characters of `file_name` contain a period
#' and you have not specified the file extension.
#' @return A tibble when the file type is a csv, tsv, xlsx, or xls, and a character vector when the file type is pdf or txt.
#' @importFrom readr read_csv
#' @importFrom readxl read_excel
#' @importFrom pdftools pdf_text
#' @export
#' @examples
#' \dontrun{
#' url <- "https://your.url.zip"
#'
#' # load the first csv within the zipped file
#' data <- read_url_zip(url)
#'
#' # load the xlsx file "data 2020.xlsx"
#' data <- read_url_zip(url, file_type = "xlsx", file_name = "data 2020")
#'
#' # load the tsv file "data.tsv"
#' data <- read_url_zip(url, file_name = "data.tsv")
#'
#' # load the 2nd txt file and use the `read_csv` function instead of the default txt reader `readLines`
#' data <- read_url_zip(url, file_type = "txt", file_index = 2, reader = "read_csv")
#' }
read_url_zip <- function(
    url,
    file_type = "csv",
    file_name = "",
    file_index = 1,
    reader = NULL,
    check_name_for_type = TRUE
  ) {

  # ensure url is a .zip url
  if (grepl(".zip", url, fixed = TRUE) == FALSE) {
    stop('\nurl = "', url, '" does not link to a zip file', call. = FALSE)
  }

  # if the last 5 characters of file_name include "." parse out file type
  ending_chars <- substr(file_name, nchar(file_name) - 4, nchar(file_name))
  if (grepl(".", ending_chars, fixed = T) & check_name_for_type == TRUE) {
    file_type <- strsplit(ending_chars, ".", fixed = T)[[1]][2]
    file_name <- gsub(paste0(".", file_type), "", file_name)
  }

  if (length(file_type) > 1) {
    stop("`file_type` must be specified or included in `file_name`", call. = FALSE)
  }

  # determine reader if not specified
  if (is.null(reader) == TRUE) {
    if (file_type %in% c("csv", "tsv")) {
      reader <- "read_csv"
    } else if (file_type %in% c("xlsx", "xls")) {
      reader <- "read_excel"
    } else if (file_type %in% c("pdf")) {
      reader <- "pdf_text"
    } else if (file_type %in% c("txt")) {
      reader <- "readLines"
    }
  }

  # if reader could not be automatically determined, stop and request reader
  if (is.null(reader) == TRUE) {
    recognized_types <- c("csv", "tsv", "xlsx", "xls", "pdf", "txt")
    stop(
      "the function name to read the file with must be specified ",
      'with `reader` when the file type is not one of the following types: "',
      paste0(recognized_types, collapse = '", "'),
      '"',
      call. = FALSE
    )
  }

  temp_dir <- tempdir()
  temp_file <- tempfile(tmpdir = temp_dir, fileext = ".zip")
  unzip_dir <- gsub(".zip", "", temp_file, fixed = TRUE)

  dir.create(unzip_dir)
  download.file(url, temp_file, quiet = TRUE)
  unzip(temp_file, exdir = unzip_dir, overwrite = TRUE)

  regex_file_type <- paste0(".", gsub(".", "", file_type, fixed = T), "$")
  regex_pattern <- paste0(
    ifelse(
      file_name == "", "",
      paste0("^", gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", file_name))
    ),
    regex_file_type
  )

  matching_files <- list.files(unzip_dir, pattern = regex_pattern, )

  if (length(matching_files) == 0) {
    available_files <- paste0(list.files(unzip_dir), collapse = '", \n"')

    unlink(temp_file, recursive = TRUE)
    unlink(unzip_dir, recursive = TRUE)

    stop(
      'There are no files matching "',
      paste0(file_name, ".", file_type),
      '"\nThe files available are: \n"',
      available_files,
      '"',
      call. = FALSE
    )
  }

  data <- do.call(get(reader), args = list(paste0(unzip_dir, .Platform$file.sep, matching_files[file_index])))

  unlink(temp_file, recursive = TRUE)
  unlink(unzip_dir, recursive = TRUE)

  return(data)
}
