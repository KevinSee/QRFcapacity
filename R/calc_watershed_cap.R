#' @title Watershed Capacity
#'
#' @description Estimate capacity within a polygon, based on line file of species domain and point estimates within the polygon
#'
#' @author Kevin See
#'
#' @param wtsd_polygon sf polygon defining the extent you wish to estimate capacity over
#' @param spp_range sf line defining the species range extent
#' @param capacity_pts sf points with estimates of capacity
#' @param capacity_name character vector of the name of the capacity column in \code{capacity_pts}
#' #' @param capacity_name character vector of the name of the column in \code{capacity_pts} which characterizes the standard error of the capacity prediction
#' @param max_snap_dist numeric value describing maximum distance (in meters) to snap points to the species range extent. Default value is \code{500}.
#' @param by_stream Should capacities be returned by stream, \code{TRUE}, or just the total across the entire watershed, \code{FALSE}. Default is \code{FALSE}
#'
#' @import dplyr sf maptools forcats
#' @return NULL
#' @export

calc_watershed_cap = function(wtsd_polygon,
                              spp_range,
                              capacity_pts,
                              capacity_name = "chnk_per_m",
                              capacity_se_name = "chnk_per_m_se",
                              max_snap_dist = 500,
                              by_stream = F) {
  
  # print(paste('Working on', wtsd_polygon$trap_name))
  
  # make sure all sf files are in same projection system
  if(st_crs(wtsd_polygon)$epsg != 5070) {
    wtsd_polygon = st_transform(wtsd_polygon,
                                crs = 5070)
  }
  
  if(st_crs(spp_range) != st_crs(wtsd_polygon)) {
    spp_range = st_transform(spp_range,
                             st_crs(wtsd_polygon))
  }
  
  if(st_crs(capacity_pts) != st_crs(wtsd_polygon)) {
    capacity_pts = st_transform(capacity_pts,
                                st_crs(wtsd_polygon))
  }
  
  # clip stream layer to watershed polygon
  wtsd_strm = st_intersection(spp_range,
                              wtsd_polygon)
  
  if(nrow(wtsd_strm) == 0) {
    print('No stream within polygon')
    wtsd_cap = tibble(area = st_area(wtsd_polygon)) %>%
      mutate(area = area / 1e6,
             area = as.numeric(area),
             tot_length = as.numeric(NA))
    return(wtsd_cap)
  }
  
  wtsd_strm = wtsd_strm %>%
    mutate(id = 1:n())
  
  # clip capacity points to watershed polygon
  wtsd_pts = st_intersection(capacity_pts,
                             wtsd_polygon)
  
  if(nrow(wtsd_pts) == 0) {
    print('No capacity points within polygon')
    wtsd_cap = tibble(area = st_area(wtsd_polygon)) %>%
      mutate(area = area / 1e6,
             area = as.numeric(area),
             n_pts = as.numeric(NA))
    return(wtsd_cap)
  }
  
  # snap capacity points to stream layer
  # cap_pts = wtsd_pts %>%
  #   as_Spatial() %>%
  #   maptools::snapPointsToLines(lines = wtsd_strm %>%
  #                                 as_Spatial,
  #                               maxDist = snap_dist,
  #                               idField = 'id') %>%
  #   st_as_sf() %>%
  #   rename(id = nearest_line_id) %>%
  #   left_join(wtsd_strm %>%
  #               as_tibble() %>%
  #               select(-geometry))
    
  cap_pts = wtsd_pts %>%
    bind_cols(wtsd_pts %>%
                as_Spatial() %>%
                maptools::snapPointsToLines(points = .,
                                            lines = wtsd_strm %>%
                                              as_Spatial,
                                            idField = 'id') %>%
                as('sf') %>%
                st_drop_geometry() %>%
                as_tibble() %>%
                select(snap_dist)) %>%
    filter(snap_dist <= max_snap_dist)
  
  
  
  # get stream length
  strm_length = wtsd_strm %>%
    mutate(lngth = st_length(.)) %>%
    group_by(StreamName) %>%
    summarise(tot_length = sum(lngth)) %>%
    mutate_at(vars(tot_length),
              list(as.numeric)) %>%
    as_tibble() %>%
    select(-geometry)
    
  # get average capacity by stream
  strm_cap = cap_pts %>%
    as_tibble() %>%
    group_by(StreamName) %>%
    summarise(n_pts = n_distinct(Site)) %>%
    full_join(cap_pts %>%
                as_tibble() %>%
                group_by(StreamName) %>%
                summarise_at(vars(cap_per_m = one_of(capacity_name),
                                  cap_per_m_se = one_of(capacity_se_name)),
                             list(mean),
                             na.rm = T)) %>%
    full_join(strm_length) %>%
    mutate(tot_cap = cap_per_m * tot_length,
           tot_cap_se = cap_per_m_se * tot_length)
  
  # add stream capacities up
  wtsd_cap = tibble(area = st_area(wtsd_polygon)) %>%
    mutate(area = area / 1e6,
           area = as.numeric(area)) %>%
    bind_cols(strm_cap %>%
                summarise_at(vars(n_pts, tot_length, tot_cap),
                             list(sum),
                             na.rm = T)) %>%
    bind_cols(strm_cap %>%
                summarise_at(vars(tot_cap_se),
                             list(~ sqrt(sum(.^2, na.rm = T)))))
  
  if(by_stream) {
    wtsd_cap = wtsd_cap %>%
      mutate(StreamName = 'Total') %>%
      left_join(strm_cap %>%
                  filter(!is.na(StreamName)) %>%
                  summarise_at(vars(cap_per_m, cap_per_m_se),
                               list(~ (weighted.mean(.,
                                                     w = tot_length,
                                                     na.rm = T)))) %>%
                  mutate(StreamName = 'Total')) %>%
      bind_rows(strm_cap) %>%
      mutate(StreamName = as.factor(StreamName),
             StreamName = fct_relevel(StreamName,
                                      'Total',
                                      after = Inf)) %>%
      select(StreamName, area, everything()) %>%
      arrange(StreamName)
  }
  
  return(wtsd_cap)
}
