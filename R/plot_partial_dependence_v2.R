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
#' @import dplyr tidyr purrr ggplot2 quantregForest ggpubr
#' @return ggplot object
#' @export

plot_partial_dependence_v2 = function(rf_mod,
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
  if(is.null(plot_covars)) plot_covars = as.character(covars)
  
  covars_type = data %>%
    select(one_of(plot_covars)) %>%
    map_chr(.f = class) %>%
    enframe(name = "covar",
            value = "type")
  
  covars_num = covars_type %>%
    filter(type %in% c("numeric", "integer")) %>%
    pull(covar)
  
  covars_fct = covars_type %>%
    filter(type  == 'factor') %>%
    pull(covar)
  
  factor_cross = data %>%
    select(one_of(covars_fct)) %>%
    crossing()
  
  # get means and ranges of all numeric covariates
  covar_range = data %>%
    select(one_of(as.character(covars_num))) %>%
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
  pdp_df = covars_type %>%
    pull(covar) %>%
    as.list() %>%
    purrr::set_names() %>%
    map_df(.id = 'Metric',
           .f = function(x) {
             
             if(class(pull(data, x)) %in% c('integer', 'numeric')) {
               df = crossing(value = seq(covar_range$min[covar_range$Metric == x],
                                         covar_range$max[covar_range$Metric == x],
                                         length.out = n_pts),
                             covar_range %>%
                               filter(Metric != x) %>%
                               select(Metric, median) %>%
                               spread(Metric, median),
                             factor_cross) %>%
                 mutate(met = value)
               
               names(df)[match('met', names(df))] = x
               
             } 
             if(class(pull(data, x)) %in% c('factor')) {
               df = crossing(factor_cross,
                             covar_range %>%
                               select(Metric, median) %>%
                               spread(Metric, median))
             }
             
             return(df)
           }) %>%
    mutate(Metric = factor(Metric,
                           levels = levels(covars)))
  
  # make predictions
  if(type == 'quantile') {
    pdp_df = pdp_df %>%
      select(Metric, value) %>%
      bind_cols(pdp_df %>%
                  select(-Metric, -value) %>%
                  mutate(pred = predict(rf_mod,
                                        newdata = .,
                                        what = pred_quantile)))
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
                select(Metric = ShortName, covar_label = Name) %>%
                distinct) %>%
    # put covariates in order by relative importance
    left_join(rel_imp %>%
                select(Metric, relImp)) %>%
    mutate_at(vars(Metric, covar_label),
              list(~ fct_reorder(., relImp))) %>%
    mutate_at(vars(Metric, covar_label),
              list(fct_rev))
  
  
  # create data.frame for rug ticks
  rug_df = data %>%
    select(Watershed, one_of(covars_num)) %>%
    gather(Metric, value, one_of(covars_num)) %>%
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
  
  
  num_p = pdp_df %>%
    filter(Metric %in% covars_num) %>%
    ggplot(aes(x = value,
               y = pred)) +
    geom_smooth(method = 'loess',
                se = F,
                aes(color = Tier1)) +
                # color = 'black') +
    geom_rug(data = rug_df %>%
               filter(Metric %in% covars_num),
             aes(x = value,
                 y = NULL),
             color = "darkgray") +
    scale_color_brewer(palette = 'Set1') +
    theme_bw() +
    theme(legend.position = 'bottom') +
    labs(x = "Covariate Value",
         y = expression(Prediction~(per~m^2))) +
    # labs(y = 'Prediction (per m)',
    #      x = 'Covariate Value',
    #      color = 'Watershed') +
    facet_wrap(~ covar_label,
               scales = scales)
  
  
  
  fct_p = pdp_df %>%
    filter(!Metric %in% covars_num) %>%
    ggplot(aes(x = Tier1,
               y = pred)) +
    geom_bar(aes(fill = Tier1),
             stat = "identity") +
    scale_fill_brewer(palette = "Set1") +
    labs(y = expression(Prediction~(per~m^2)))
  
  my_p = ggpubr::ggarrange(plotlist = list(num_p, fct_p),
                           nrow = 1,
                           common.legend = T,
                           legend = "bottom")
  
  return(my_p)
  
}
