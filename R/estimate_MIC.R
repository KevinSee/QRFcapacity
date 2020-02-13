#' @title Estimate MIC
#'
#' @description Estimate various MIC-related statistics
#'
#' @author Kevin See
#'
#' @param data data.frame containing response as one column, and various covariates as other columns
#' @param covars vector of potential covariate names, which match column names in \code{data}
#' @param response name of response, which matches one column name in \code{data}
#'
#' @import dplyr minerva
#' @return NULL
#' @export

estimate_MIC = function(data,
                        covars,
                        response) {
  
  if (!requireNamespace("minerva", quietly = TRUE)) {
    stop("Package \"minerva\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  res = minerva::mine(x = data %>%
               dplyr::select(one_of(covars)),
             y = data %>%
               pull(response),
             use = 'pairwise.complete.obs') %>%
    map_df(.f = identity) %>%
    mutate(Metric = data %>%
             dplyr::select(one_of(covars)) %>%
             names()) %>%
    dplyr::select(Metric, everything()) %>%
    left_join(data %>%
                dplyr::select(one_of(covars)) %>%
                gather(Metric, value) %>%
                group_by(Metric) %>%
                summarise(var = var(value, na.rm = T),
                          obsCV = sd(value, na.rm = T) / abs(mean(value, na.rm = T)),
                          non_NA = sum(!is.na(value)),
                          is_NA = sum(is.na(value)),
                          perc_NA = round(is_NA / (is_NA + non_NA), 3),
                          non_0 = sum(value != 0, na.rm = T),
                          perc_non0 = non_0 / n()),
              by = 'Metric') %>%
    ungroup() %>%
    dplyr::select(Metric, var:perc_non0, everything())
  
  return(res)
}
