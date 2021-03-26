#' descriptive_table
#'
#' This function uses the psych::corr.test function to generated the correlation. One potential problem with the current version of the function is that if the correlation between the two item is too high (rounded as 1), then that cell will become blank. This is a very rare problem, and I have not yet figure out how to solve this.
#'
#' @param data a dataframe
#' @param cols vector or quos(). column(s) that need to be recoded
#' @param cor_sig_test adjusted or raw. Default as adjusted. See psych::corr.test to learn more.
#' @param cor_digit number of digit for correlation table
#' @param mean_sd_digit number of digit for mean and sd tables
#' @param filepath provide full path to pass into the write.csv(file = filepath)
#'
#' @return
#' @export
#'
#' @examples
#' descriptive_table(iris,cols = tidyr::everything())

descriptive_table =  function(data,cols,cor_sig_test = 'raw',cor_digit = 3,mean_sd_digit = 3, filepath = NULL) {
  cols = ggplot2::enquo(cols)
  data = data %>% dplyr::select(!!cols)

  datatype = as.vector(sapply(data, class))
  if(all(datatype == 'numeric'| datatype == 'factor' | datatype == 'integer')){
    data = data %>% dplyr::mutate(dplyr::across(tidyr::everything(),as.numeric))
  } else{
    print('Error: All columns must be dummy coded or factored. Consider using as.factor() or as.numeric()')
    return()
  }

  mean_table = data %>% dplyr::summarise(dplyr::across(!!cols, ~ mean(., na.rm = T))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ format(round(., mean_sd_digit),nsmall = mean_sd_digit))) %>%
    tidyr::pivot_longer(cols = tidyr::everything(),names_to = 'rowname',values_to = 'mean')

  sd_table = data %>% dplyr::summarise(dplyr::across(!!cols, ~ stats::sd(., na.rm = T))) %>%
    dplyr::mutate(dplyr::across(where(is.numeric), ~ format(round(., mean_sd_digit),nsmall = mean_sd_digit))) %>%
    tidyr::pivot_longer(cols = tidyr::everything(),names_to = 'rowname',values_to = 'sd')

  cor_table = data %>% cor_test(cols = !!cols, sig_test = cor_sig_test,digit = cor_digit,descriptive_table_use = T)

  return_df = mean_table %>%
    dplyr::full_join(sd_table,by = 'rowname') %>%
    dplyr::full_join(cor_table,by = 'rowname') %>%
    tibble::column_to_rownames('rowname')

  if (!is.null(filepath)) {
    write.csv(x = return_df, file = filepath)
  }
  return(return_df)
}
