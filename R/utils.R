

#' Define attributes of subject property
#'
#' @param df data frame containing subject property
#' @param subject_apn APN of subject property
#'
#' @return assigns the attributes to the global environment
#' @export
#'
#' @examples
define_subject <- function(df, subject_apn){
  assertthat::assert_that(assertthat::has_name(df, 'APN'), msg = 'APN column needs to be spelt as follow: APN')

  subj_data <- df %>%
    dplyr::filter(APN == subject_apn)

  assign('subject_lat', subj_data$Latitude, envir = .GlobalEnv)
  assign('subject_lng', subj_data$Longitude, envir = .GlobalEnv)
  assign('subject_apn', subject_apn, envir = .GlobalEnv)
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
#' @examples reproject_latlon(sales_with_knn, old_crs = 3502, new_crs = 4326)
reproject_latlon <- function(df, old_crs = 4326, new_crs = 3502){
  df %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = old_crs) %>%
    # https://spatialreference.org/ref/epsg/3502/
    sf::st_transform(new_crs) %>%
    dplyr::mutate(lon = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
    sf::st_set_geometry(NULL)
}

hist_plot <- function(df, title_lab, x_lab, specific_variable){
  ggplot2::ggplot(data = df, ggplot2::aes_string(specific_variable)) +
    ggplot2::geom_histogram(colour="black", fill="blue") +
    ggplot2::labs(title = title_lab, x = x_lab, y = "Count", caption = "Valuemetrics.info SGDS2")
}
