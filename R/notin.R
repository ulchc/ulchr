#' Negate value matching
#'
#' An intuitive counterpart to `%in%` which returns a logical
#' vector indicating if the left operand is \emph{not in} the right operand.
#' @param abc Vector or NULL: the values to be matched.
#' @param xyz Vector or NULL: the values to be matched against.
#' @return A vector of the same length as x.
#' @export
#' @seealso [base::match()] [base::Negate()]
#' @examples
#' 1:10 %notin% c(1,3,5,9)
`%notin%` <- function(abc, xyz) base::match(abc, xyz, nomatch = 0) == 0
