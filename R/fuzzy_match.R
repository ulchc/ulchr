#' Fuzzy match two character vectors
#'
#' Substitutes all elements of `swap_out` with one from `swap_in`. All
#' perfect matches are immediately substituted, with proceeding matches
#' assigned based on the smallest string distance computed across the remainder
#' of `swap_out` and `swap_in`.
#'
#' When two or more matches have equivalent total string distances, the `swap_in`
#' term with the maximum variance across all remaining `swap_out` terms will proceed.
#' In doing so, the most distinct term is removed from the set of potential matches
#' so as to leave the terms with the most overlap to the end of the matching sequence.
#'
#' @param swap_out A vector of characters.
#' @param swap_in A vector of characters >= `length(swap_out)`.
#'
#' @details Implements the running cosine matching algorithm from [stringdist::afind()]
#' with a preference for `q = 3`, but constrained to the minimum number of characters
#' of any string in `swap_out` or `swap_out`.
#'
#' @return A character vector equal in length to `swap_out`.
#'
#' @importFrom stringdist afind
#' @importFrom stringi stri_replace_rstr
#' @export
#' @examples
#' messy_words <- c(
#'   "ID #", # ID
#'   "Barcode", # Code
#'   "Product\nName", # Name
#'   "Day of \n the week", # Day
#'   "Month (MMM) ", # Month
#'   "Amount $" # Amount
#' )
#'
#' clean_words <- c(
#'   "Amount",
#'   "Month ",
#'   "Day",
#'   "Name",
#'   "Code",
#'   "ID"
#' )
#'
#' swapped_words <- fuzzy_match(
#'   swap_out = messy_words,
#'   swap_in = clean_words
#' )
#' swapped_words # order preserved, best matches substituted
#'
#'
#' # categorize and handle spelling mistakes from OCR text
#' color_phrases <- c(
#'   "The sunrise was 'yellovv'",
#'   "There were 'purp/e' flowers",
#'   "The fruit was 'orang e'"
#' )
#'
#' # swap_in can be any length >= swap_out
#' colors_list <- c(
#'   "Red",
#'   "Blue",
#'   "Green",
#'   "Yellow",
#'   "Violet",
#'   "Purple",
#'   "Orange"
#' )
#'
#' # a notice will be shown where there is a potential incompatible fuzzy match
#' colors_mentioned <- fuzzy_match(color_phrases, colors_list)
#'
#' writeLines(paste0(
#'   "The colors mentioned were: ",
#'   paste0(colors_mentioned, collapse = ", ")
#' ))
fuzzy_match <- function(swap_out, swap_in) {
  if (length(swap_out) > length(swap_in)) {
    warning("Length of swap_out cannot be greater than swap_in")
  }
  dissimilar_matches <- list()
  origin_list <- swap_out
  new_list <- rep(NA, length(origin_list))

  index_perf_out <- which(swap_out %in% swap_in)
  index_perf_in <- which(swap_in %in% swap_out)

  if (length(index_perf_out) != 0) {
    new_list[index_perf_out] <- swap_out[index_perf_out]
    writeLines("> Perfect Matches")
    for (item in new_list[!is.na(new_list)]) {
      message(paste0("`", item, "`"))
    }
    swap_out <- swap_out[-index_perf_out]
    swap_in <- swap_in[-index_perf_in]
  }

  writeLines("> Fuzzy Matches")
  while (length(swap_out) != 0) {

    # remove special characters
    proc_swap_out <- str_replace_all(tolower(swap_out), "[^[:alnum:]]", " ")
    proc_swap_in <- str_replace_all(tolower(swap_in), "[^[:alnum:]]", " ")

    # prefer q = 3, but do not exceed nchar of any term (prevent NaN)
    bounded_q <- min(3, nchar(c(proc_swap_out, proc_swap_in)))

    # compute distances between all terms in both lists
    out_mat <- afind(proc_swap_out, proc_swap_in,
                     method = "running_cosine",
                     q = bounded_q
    )

    # find the minimum distance of any two terms
    min_dist <- min(out_mat$distance)
    # find the matrix indices for any minimum distance match
    min_dist_indices <- which(out_mat$distance == min_dist)

    # if there is a single min_dist index or all matches are completely incompatible
    if (length(min_dist_indices) == 1 | length(min_dist_indices) == length(out_mat$distance)) {
      most_distinct_index <- min_dist_indices[1]
    } else {
      # if more than one index, find the column indices of the swap in terms
      min_dist_cols <- ceiling(min_dist_indices / length(proc_swap_out))
      # find most distinct fuzzy match having the max sd across all terms (few similarities with other terms)
      most_distinct_index <- min_dist_indices[which.max(apply(out_mat$distance[, min_dist_cols], 2, sd))]
    }

    # find the column index of the swap_in term
    swap_in_index <- ceiling(most_distinct_index / length(proc_swap_out))

    # find the row index of the swap_out term
    swap_out_index <- ifelse(
      most_distinct_index %% length(proc_swap_out) == 0,
      length(proc_swap_out),
      most_distinct_index %% length(proc_swap_out)
    )

    loc_push_new <- grep(
      stringi::stri_replace_rstr(swap_out[swap_out_index]),
      origin_list
    )

    if (length(loc_push_new) == 0) {
      loc_push_new <- grep(stringi::stri_replace_rstr(swap_out[swap_out_index]),
                           origin_list,
                           fixed = TRUE
      )
    }

    if (min_dist > 0.9) {
      dissimilar_matches <- append(
        dissimilar_matches,
        paste0("`", swap_out[swap_out_index], "` may have a dissimilar fuzzy match")
      )
    }

    new_list[loc_push_new] <- swap_in[swap_in_index]

    message(
      gsub(
        "\\\n", "\\\\n",
        gsub(
          "\\\r", "\\\\r",
          paste0(
            "`", swap_out[swap_out_index],
            "` -> `", new_list[loc_push_new], "`"
          )
        )
      )
    )

    swap_out <- swap_out[-swap_out_index]
    swap_in <- swap_in[-swap_in_index]
  }
  if (length(dissimilar_matches) != 0) {
    writeLines("> Incompatible?")
    message(paste0(gsub("\\\n", "\\\\n", unlist(dissimilar_matches)),
                   collapse = "\n"
    ))
  }

  return(new_list)
}
