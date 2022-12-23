#' Quickly view data with a DT::datatable
#'
#' View your data while you code without switching to a tab created by View().
#'
#' @param df A data frame or tibble.
#' @return A DT::datatable of length nrow(df) - 1.
#' @export
#' @seealso [DT::datatable()]
view_DT <- function(df, randomize_index = FALSE, line_height = '50%', max_cells = 1000000, verbose = FALSE) {

  index <- 1:nrow(df)

  if (randomize_index == TRUE) {
    index <- sample(index)
  }

  if (nrow(df)*ncol(df) > max_cells) {
    if (verbose == TRUE) {
      warning(
        glue::glue('Data has more cells than `max_cells` = {scales::comma(max_cells)}. '),
        'A random sample of rows will be displayed instead of all rows. ',
        'Change `max_cells` if you want to render a larger widget.'
      )
    }
    index <- sample(min(max_cells, max(index)))
  }

  df_DT <- DT::datatable(
    df[index,],
    rownames = FALSE,
    extensions = "Buttons",
    options = list(
      pageLength = nrow(df) - 1,
      dom = "B",
      buttons = c("excel")
    )
  )

  df_DT <- DT::formatStyle(
    df_DT,
    names(df),
    lineHeight = line_height
  )

  return(df_DT)
}
