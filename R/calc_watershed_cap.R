#' @title Watershed Capacity
#'
#' @description Estimate capacity within a polygon, based on line file of species domain and point estimates within the polygon
#'
#' @author Kevin See
#'
#' @param wtsd_polygon sf polygon defining the extent you wish to estimate capacity over
#' @param capacity_sf sf file of either points or lines with estimates of capacity
#' @param capacity_name character vector of the name of the capacity column in \code{capacity_pts}
#' @param capacity_se_name character vector of the name of the column in \code{capacity_pts} which characterizes the standard error of the capacity prediction
#' @param spp_range sf line defining the species range extent. Only needed if \code{capacity_sf} is a point file.
#' @param max_snap_dist numeric value describing maximum distance (in meters) to snap points to the species range extent. Default value is \code{500}. Only needed if \code{capacity_sf} is a point file.
#' @param by_stream Should capacities be returned by stream, \code{TRUE}, or just the total across the entire watershed, \code{FALSE}. Default is \code{FALSE}
#'
#' @import dplyr sf tibble maptools forcats
#' @return NULL
#' @export

calc_watershed_cap = function(wtsd_polygon,
                              capacity_sf,
                              capacity_name = "chnk_per_m",
                              capacity_se_name = "chnk_per_m_se",
                              spp_range = NULL,
                              max_snap_dist = 500,
                              by_stream = F) {
  
  # make sure all sf files are in same projection system
  if(st_crs(wtsd_polygon)$epsg != 5070) {
    wtsd_polygon = st_transform(wtsd_polygon,
                                crs = 5070)
  }
  
  if(st_crs(capacity_sf) != st_crs(wtsd_polygon)) {
    capacity_sf = st_transform(capacity_sf,
                                st_crs(wtsd_polygon))
  }
  
  if(st_geometry_type(capacity_sf, F) %in% c("POINT", "MULTIPOINT")) {
  
    if(st_crs(spp_range) != st_crs(wtsd_polygon)) {
      spp_range = st_transform(spp_range,
                               st_crs(wtsd_polygon))
    }

    # clip stream layer to watershed polygon
    wtsd_strm = st_intersection(spp_range,
                                wtsd_polygon) %>%
      st_cast("MULTILINESTRING")
    
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
    wtsd_pts = st_intersection(capacity_sf,
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
                  select(id = nearest_line_id,
                         snap_dist)) %>%
      filter(snap_dist <= max_snap_dist) %>%
      left_join(wtsd_strm %>%
                  as_tibble() %>%
                  select(id, StreamName)) %>%
      select(-id)
    
    
    
    # get stream length
    strm_length = wtsd_strm %>%
      mutate(lngth = st_length(.)) %>%
      group_by(StreamName) %>%
      summarise(tot_length = sum(lngth),
                .groups = "drop") %>%
      mutate_at(vars(tot_length),
                list(as.numeric)) %>%
      st_drop_geometry() %>%
      as_tibble()
    
    # get average capacity by stream
    strm_cap = cap_pts %>%
      as_tibble() %>%
      group_by(StreamName) %>%
      summarise(n_pts = n_distinct(Site),
                .groups = "drop") %>%
      full_join(cap_pts %>%
                  as_tibble() %>%
                  select(StreamName,
                         cap_per_m = one_of(capacity_name),
                         cap_per_m_se = one_of(capacity_se_name)) %>%
                  group_by(StreamName) %>%
                  summarize(across(c(cap_per_m,
                                     cap_per_m_se),
                                   mean,
                                   na.rm = T),
                            .groups = "drop")) %>%
      full_join(strm_length) %>%
      mutate(across(n_pts,
                    replace_na,
                    0)) %>%
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
    
  } else if(st_geometry_type(capacity_sf, F) %in% c("LINESTRING", "MULTILINESTRING")) {
    
    # clip stream layer to watershed polygon
    wtsd_strm = capacity_sf %>%
      rename(cap_per_m = any_of(capacity_name),
             cap_per_m_se = any_of(capacity_se_name),
             spp_domain = any_of(str_sub(capacity_name, 1, 4))) %>%
      st_intersection(wtsd_polygon) %>%
      mutate(cap_rch = if_else(spp_domain,
                               reach_leng * cap_per_m,
                               0),
             cap_rch_se = if_else(spp_domain,
                                  reach_leng * cap_per_m_se,
                                  0))
    
    wtsd_cap = wtsd_strm %>%
      filter(spp_domain) %>%
      summarise(n_rchs = n_distinct(UniqueID),
                tot_length = sum(reach_leng),
                tot_cap = sum(cap_rch),
                tot_cap_se = sqrt(sum(cap_rch_se^2))) %>%
      st_drop_geometry() %>%
      as_tibble()
    
    if(by_stream) {
      wtsd_cap = wtsd_cap %>%
        tibble::add_column(GNIS_Name = "Total",
                   .before = 0) %>%
        bind_rows(wtsd_strm %>%
                    filter(spp_domain) %>%
                    st_drop_geometry() %>%
                    as_tibble() %>%
                    group_by(GNIS_Name) %>%
                    summarise(n_rchs = n_distinct(UniqueID),
                              tot_length = sum(reach_leng),
                              tot_cap = sum(cap_rch),
                              tot_cap_se = sqrt(sum(cap_rch_se^2)),
                              .groups = "drop")) %>%
        rename(StreamName = GNIS_Name) %>%
        mutate(StreamName = as.factor(StreamName),
               StreamName = fct_relevel(StreamName,
                                        'Total',
                                        after = Inf)) %>%
        select(StreamName, everything()) %>%
        arrange(StreamName)
    }
  }
  
  return(wtsd_cap)
}
