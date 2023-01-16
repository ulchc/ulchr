#' A `ggplot2` Theme
#'
#' Returns a modified `theme_ipsum()` with a dotted grid and smaller line size.
#'
#' @return A modified `theme_ipsum()` from hrbrthemes
#'
#' @importFrom hrbrthemes theme_ipsum
#' @importFrom ggplot2 `%+replace%`
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_line
#' @export
#' @examples
#' library(ggplot2)
#'
#' ggplot(iris, aes(x = Sepal.Width)) +
#'   geom_bar() +
#'   theme_ipsum_dot()
theme_ipsum_dot <- function() {
  suppressWarnings({
    ggplot2::`%+replace%`(
      hrbrthemes::theme_ipsum(),
      ggplot2::theme(
        panel.grid.major = ggplot2::element_line(
          linetype = "dotted",
          colour = "#454545",
          size = 0.3),
        panel.grid.minor = ggplot2::element_line(
          linetype = "dotted",
          colour = "#454545",
          size = 0.3),
        axis.line = ggplot2::element_line(color = "#454545", size = 0.3)
      )
    )
  })
}
