

#' Define attributes of subject property
#'
#' @param subject_lat The subject properties latitude
#' @param subject_lng The subject properties longitude
#'
#' @return assigns the attributes to the global environment
#' @export
#'
#' @examples
define_subject <- function(subject_lat, subject_lng){
  assign('subject_lat', subject_lat, envir = .GlobalEnv)
  assign('subject_lng', subject_lng, envir = .GlobalEnv)
}

# De-mean lat and lon coords such that their rescaled values will be relative
# to one another (distance from a shared center point)
demean_coords <- function(x, y) {
  demeaned_coords <- c((x - mean(x)), y - mean(y))
  range(demeaned_coords)
}



# Rescale numeric vector to be between 0 and 1
# Taken from the scales library
rescale <- function(x, to = c(0, 1), from = range(x, na.rm = T, finite = T)) {
  (x - from[1]) / diff(from) * diff(to) + to[1]
}


#' Reproject lat/lon into planar coordinates
#'
#' @param df data frame containing Latitude and Longitude
#'
#' @return returns data frame with Latitude and Longitude reprojected into EPSG 3502 (central colorado)
#' @export
#'
#' @examples
reproject_latlon <- function(df, old_crs = 4326, new_crs = 3502){
  df %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = old_crs) %>%
    # https://spatialreference.org/ref/epsg/3502/
    sf::st_transform(new_crs) %>%
    dplyr::mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
    sf::st_set_geometry(NULL)
}


