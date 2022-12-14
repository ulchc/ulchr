% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_url_zip.R
\name{read_url_zip}
\alias{read_url_zip}
\title{Load data from online zipped folders}
\usage{
read_url_zip(
  url,
  file_type = "csv",
  file_name = "",
  file_index = 1,
  reader = NULL,
  check_name_for_type = TRUE
)
}
\arguments{
\item{url}{The download link to a zipped folder online (ending with .zip).}

\item{file_type}{A character vector indicating a file extension.}

\item{file_name}{An optional string to specify which file to load using the file name.}

\item{file_index}{An optional index number to specify which file to load when \code{file_name} isn't specified.}

\item{reader}{A string indicating which function to use when reading the file into R.}

\item{check_name_for_type}{Boolean option to prevent detecting \code{file_type} from
the last 5 characters of \code{file_name}. Use if the last few characters of \code{file_name} contain a period
and you have not specified the file extension.}
}
\value{
A tibble when the file type is a csv, tsv, xlsx, or xls, and a character vector when the file type is pdf or txt.
}
\description{
Downloads the zip folder pointed to by \code{url}, unzips it to a temporary
location, reads the first csv into R as a tibble, then deletes the downloaded
zip folder and the temporary files.
}
\details{
\code{file_type}, \code{file_name}, and \code{file_index} can be changed from the defaults
to read a file that is not the first csv. If the file specified is not a "csv",
"tsv", "xlsx", "xls", "pdf", or "txt", a function to read the file with
must be specified using the \code{reader} parameter.
}
\examples{
\dontrun{
url <- "https://your.url.zip"

# load the first csv within the zipped file
data <- read_url_zip(url)

# load the xlsx file "data 2020.xlsx"
data <- read_url_zip(url, file_type = "xlsx", file_name = "data 2020")

# load the tsv file "data.tsv"
data <- read_url_zip(url, file_name = "data.tsv")

# load the 2nd txt file and use the `read_csv` function instead of the default txt reader `readLines`
data <- read_url_zip(url, file_type = "txt", file_index = 2, reader = "read_csv")
}
}
