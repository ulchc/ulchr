#' Combine columns of tibbles ignoring column length
#'
#' Equates the number of rows between two tibbles (by inserting NA rows)
#' and returns the column-combined result.
#'
#' @param df1 A data frame or tibble.
#' @param df2 A data frame or tibble.
#' @return A data frame or tibble.
#' @export
cbind_ignore_row <- function(df1, df2) {
  if (nrow(df1) > nrow(df2)) {
    pad_num <- nrow(df1) - nrow(df2)
    pad <- df2[1:pad_num, ]
    pad[1:pad_num, ] <- NA
    df2 <- df2 %>% bind_rows(pad)
  } else if (nrow(df2) > nrow(df1)) {
    pad_num <- nrow(df2) - nrow(df1)
    pad <- df1[1:pad_num, ]
    pad[1:pad_num, ] <- NA
    df1 <- df1 %>% bind_rows(pad)
  }

  combined_df <- bind_cols(df1, df2)
  return(combined_df)
}
