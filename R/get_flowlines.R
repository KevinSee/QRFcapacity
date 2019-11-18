#' @title Get Flowlines
#'
#' @description Download flowlines with NHD flowline data for mapping purposes. I modified this from here: https://ryanpeek.github.io/2017-11-05-mapping-with-sf-Part-2/.
#'
#' @author Kevin See
#'
#' @param stream_order maximum Strahler order of streams to return
#' @param wtsd_polygon sf polygon describing the boundry of where stream lines should be found
#'
#' @import httr sf
#' @return NULL
#' @export


get_flowlines = function(stream_order = 3, 
                         wtsd_polygon = NULL) {
  
  if(is.null(wtsd_polygon)) stop('Polygon is missing')
  
  # generate bounding box, in lat/long
  bbox = wtsd_polygon %>%
    st_transform(crs = 4326) %>%
    st_bbox()
  
  postURL <- "https://cida.usgs.gov/nwc/geoserver/nhdplus/ows"
  
  filterXML <- paste0('<?xml version="1.0"?>',
                      '<wfs:GetFeature xmlns:wfs="http://www.opengis.net/wfs" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:gml="http://www.opengis.net/gml" service="WFS" version="1.1.0" outputFormat="shape-zip" xsi:schemaLocation="http://www.opengis.net/wfs http://schemas.opengis.net/wfs/1.1.0/wfs.xsd">',
                      '<wfs:Query xmlns:feature="https://gov.usgs.cida/nhdplus" typeName="feature:nhdflowline_network" srsName="EPSG:4326">',
                      '<ogc:Filter xmlns:ogc="http://www.opengis.net/ogc">',
                      '<ogc:And>',
                      '<ogc:PropertyIsGreaterThan>',
                      '<ogc:PropertyName>streamorde</ogc:PropertyName>',
                      '<ogc:Literal>',stream_order - 1,'</ogc:Literal>',
                      '</ogc:PropertyIsGreaterThan>',
                      '<ogc:BBOX>',
                      '<ogc:PropertyName>the_geom</ogc:PropertyName>',
                      '<gml:Envelope>',
                      '<gml:lowerCorner>',bbox[2]," ",bbox[1],'</gml:lowerCorner>',
                      '<gml:upperCorner>',bbox[4]," ",bbox[3],'</gml:upperCorner>',
                      '</gml:Envelope>',
                      '</ogc:BBOX>',
                      '</ogc:And>',
                      '</ogc:Filter>',
                      '</wfs:Query>',
                      '</wfs:GetFeature>')
  
  destination = file.path(tempdir(),"nhdflowline_network.zip")
  file <- httr::POST(postURL, 
                     body = filterXML, 
                     httr::write_disk(destination, overwrite=T))
  
  file_path <- tempdir()
  print("unzipping...")
  unzip(destination, exdir = file_path)
  
  flow_lines <- sf::st_read(file_path, 
                            layer = 'nhdflowline_network',
                            quiet = T) %>%
    st_transform(st_crs(wtsd_polygon)) %>%
    st_intersection(wtsd_polygon)
  
  return(flow_lines)
}
