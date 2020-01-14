#' @title Partial Dependence Plots
#'
#' @description Plot the partial dependence of the response for each covariate
#'
#' @author Kevin See
#'
#' @param rf_mod the \code{quantregForest} model 
#' @param data the data.frame used to fit the \code{rf_mod}
#' @param plot_covars character vector of covariates to plot
#' @param data_dict data.frame containing columns \code{}
#' @param type whether to predict a particular quantile, or the mean
#' @param pred_quantile if \code{type} is "quantile", which quantile to predict, between 0 and 1? The default value is 0.9
#' @param n_pts how many points to use in predictions? 
#' @param log_transform was the response log transformed? 
#' @param log_offset if an offset was used before log transforming, enter it here.
#' @param scales ggplot facet_wrap argument \code{scales} for facetting
#' @param ... other arguements to be passed to \code{ggplot}
#'
#' @import dplyr tidyr purrr ggplot2 quantregForest
#' @return ggplot object
#' @export

plot_partial_dependence = function(rf_mod,
                                   data,
                                   plot_covars = NULL,
                                   data_dict = NULL,
                                   type = c('quantile', 'mean'),
                                   pred_quantile = 0.9,
                                   n_pts = 200,
                                   log_transform = T,
                                   log_offset = 0.005,
                                   scales = 'free') {
  
  if(is.null(data_dict)) {
    data(hab_dict_2017)
    data_dict = hab_dict_2017
  }
  
  type = match.arg(type)
  
  # relative importance of covariates
  rel_imp = as_tibble(rf_mod$importance,
                      rownames = 'Metric') %>%
    mutate(relImp = IncNodePurity / max(IncNodePurity)) %>%
    mutate_at(vars(Metric),
              list(~ fct_reorder(., relImp))) %>%
    arrange(desc(Metric))
  
  # names of covariates
  covars = rel_imp$Metric
  if(is.null(plot_covars)) plot_covars = covars
  
  # get means and ranges of all
  covar_range = data %>%
    select(one_of(as.character(plot_covars))) %>%
    gather(Metric, value) %>%
    group_by(Metric) %>%
    summarise_at(vars(value),
                 list(mean = mean, 
                      median = median, 
                      min = min, 
                      max = max),
                 na.rm = T) %>%
    ungroup()
  
  # create data.frame with sequence of values across each covariate, keeping other covariates at their average value
  pdp_df = covar_range %>%
    split(.$Metric) %>%
    map_df(.id = 'Metric',
           .f = function(x) {
             
             if(class(pull(data, x$Metric)) %in% c('integer', 'numeric')) {
               df = crossing(value = seq(x$min,
                                         x$max,
                                         length.out = n_pts),
                             covar_range %>%
                               filter(Metric != x$Metric) %>%
                               select(Metric, median) %>%
                               spread(Metric, median)) %>%
                 mutate(met = value)
               
               names(df)[match('met', names(df))] = x$Metric
               
             } 
             if(class(pull(data, x$Metric)) %in% c('factor')) {
               df = crossing(value = data %>%
                               select(x$Metric) %>%
                               levels,
                             covar_range %>%
                               filter(Metric != x$Metric) %>%
                               select(Metric, median) %>%
                               spread(Metric, median)) %>%
                 mutate(met = value)
               
               names(df)[match('met', names(df))] = x$Metric
             }
             
             return(df)
           }) %>%
    mutate(Metric = factor(Metric,
                           levels = levels(covars)))
  
  # make predictions
  if(type == 'quantile') {
    pdp_df = pdp_df %>%
      mutate(pred = predict(rf_mod,
                            newdata = .,
                            what = pred_quantile))
  }
  if(type == 'mean') {
    pdp_df = pdp_df %>%
      mutate(pred = predict(rf_mod,
                            newdata = .,
                            what = mean))
  }
  
  # transform predictions back from log-scale if necessary
  if(log_transform) {
    pdp_df = pdp_df %>%
      mutate_at(vars(pred),
                list(~ exp(.) - log_offset))
  }
  
  # get covariate labels
  pdp_df = pdp_df %>%
    left_join(data_dict %>%
                select(Metric = ShortName, covar_label = Name)) %>%
    # put covariates in order by relative importance
    left_join(rel_imp %>%
                select(Metric, relImp)) %>%
    mutate_at(vars(Metric, covar_label),
              list(~ fct_reorder(., relImp))) %>%
    mutate_at(vars(Metric, covar_label),
              list(fct_rev))
  
  
  # create data.frame for rug ticks
  rug_df = data %>%
    select(Watershed, one_of(as.character(covars))) %>%
    gather(Metric, value, one_of(as.character(covars))) %>%
    left_join(data_dict %>%
                select(Metric = ShortName, 
                       covar_label = Name)) %>%
    # put covariates in order by relative importance
    left_join(rel_imp %>%
                select(Metric, relImp)) %>%
    mutate_at(vars(Metric, covar_label),
              list(~ fct_reorder(., relImp))) %>%
    mutate_at(vars(Metric, covar_label),
              list(fct_rev)) %>%
    select(Metric, covar_label, Watershed, value)
  
  
  my_p = pdp_df %>%
    filter(Metric %in% plot_covars) %>%
    ggplot(aes(x = value,
               y = pred)) +
    geom_smooth(method = 'loess',
                se = F,
                color = 'black') +
    geom_rug(data = rug_df %>%
               filter(Metric %in% plot_covars), 
             aes(x = value,
                 y = NULL,
                 color = Watershed)) +
    scale_color_brewer(palette = 'Set3') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    labs(y = 'Prediction (per m)',
         x = 'Covariate Value',
         color = 'Watershed') +
    facet_wrap(~ covar_label,
               scales = scales)
  
  return(my_p)
  
}
