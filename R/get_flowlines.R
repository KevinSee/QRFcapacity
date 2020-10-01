#' @title Get Flowlines
#'
#' @description Download flowlines with NHD flowline data for mapping purposes.
#'
#' @author Kevin See
#'
#' @param stream_order maximum Strahler order of streams to return
#' @param wtsd_polygon sf polygon describing the boundry of where stream lines should be found
#'
#' @import sf nhdplusTools
#' @return NULL
#' @export


get_flowlines = function(stream_order = 3, 
                         wtsd_polygon = NULL) {
  
  if(is.null(wtsd_polygon)) stop('Polygon is missing')
  
  # generate bounding box, in lat/long
  bbox = wtsd_polygon %>%
    st_bbox()
  
  nhd_lst = nhdplusTools::plot_nhdplus(bbox = bbox,
                                       streamorder = stream_order,
                                       actually_plot = F)
  
  flow_lines = nhd_lst$flowline %>%
    sf::st_zm() %>%
    st_transform(crs = st_crs(wtsd_polygon)) %>%
    sf::st_intersection(wtsd_polygon)
  
  return(flow_lines)
}
