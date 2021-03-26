#' Count number of non-missing / non-NA values
#'
#' Center all columns with respect to the grand mean
#'
#' @param data dataframe
#' @param cols vector or tidyselect syntax or helpers. column(s) that need to be centered
#' @param group character. pass to group_by
#' @param print return the original data, and print the summary
#'
#' @return
#' return a dataframe with the number of non-NA values
#' @export
#'
#' @examples
#'
summarize_non_missing_values = function(data, cols, group = NULL,print = F) {
  cols = ggplot2::enquo(cols)
  if (!is.null(group)) {
    return_df = data %>%
      dplyr::group_by(!!group) %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.))))
  } else {
    return_df = data %>%
      dplyr::summarize(dplyr::across(!!cols, ~ sum(!is.na(.)))) %>%
      tidyr::pivot_longer(tidyr::everything())
  }

  if (print) {
    print(return_df)
    return(data)
  } else {
    return(return_df)
  }
}
