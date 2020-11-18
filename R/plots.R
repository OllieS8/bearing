#' Create a leaflet plot of the subject and comparable properties
#'
#' @param df Data frame with comparable properties
#' @param subject_lng subject longitude
#' @param subject_lat subject latitude
#'
#' @return returns a leaflet plot with subject and comparable properties
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' leaflet_plot(Apartments2)
leaflet_plot <- function(df, subject_lng = NULL, subject_lat = NULL){
  assertthat::assert_that(assertthat::has_name(df, 'Latitude'), msg = 'Latitude column needs to be spelt as follow: Latitude')
  assertthat::assert_that(assertthat::has_name(df, 'Longitude'), msg = 'Longitude column needs to be spelt as follow: Longitude')

  tryCatch({
    subject_lng <- get('subject_lng')
  },
  error=function(cond){
    message('Error: Must run function define_subject before leaflet plot')
  })

  tryCatch({
    subject_lat <- get('subject_lat')
  },
  error=function(cond){
    message('Error: Must run function define_subject before leaflet plot')
  })

  options(scipen=999)

  leaflet::leaflet(data = df) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(layerId = "subj", lng = subject_lng, lat = subject_lat, popup = "Subject Property", radius = 10, color = "red",
                     weight = 5, opacity = 1, fill = TRUE, fillColor = "red",
                     fillOpacity = 1, dashArray = NULL,  popupOptions = NULL, label = NULL, labelOptions = NULL, options = leaflet::pathOptions(), clusterOptions = NULL, clusterId = NULL) %>%
    leaflet::addMarkers(lng = ~Longitude, lat = ~Latitude, popup = paste("Address", df$Address, "<br>",
                                                                        "Sale Price: $", df$PriceSold, "<br>"))
}

#' Create a histogram of different sales metrics
#'
#' @param df Data frame
#' @param columns number of columns for plots - default = 1
#' @param variable variable of interest: PriceSold, PPSF, PPBR or PPUnit.
#'
#' @return returns a histogram
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' sales_price_histogram(Apartments3, variable = 'PriceSold', bin_width = 100000, xlimits = c(0,7500000))
sales_price_histogram <- function(df, variable = c("All", "PriceSold","PPSF","PPBR","PPUnit"), columns = 1){

  p <- list()

  if('PriceSold' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PriceSold'), msg = 'PriceSold column needs to be spelt as follow: PriceSold')
    specific_variable <- 'PriceSold'
    title_lab <- "Histogram of Sold Price"
    x_lab <- "Reported Sold Price"
    p1 <- bearing::hist_plot(df, title_lab, x_lab, specific_variable)
    p <- c(p, p1 = list(p1))
  }

  if('PPSF' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PPSF'), msg = 'PPSF column needs to be spelt as follow: PPSF')
    specific_variable <- 'PPSF'
    title_lab <- "Histogram of Sold Price Per Sq Ft"
    x_lab <- "Reported Sold Price/SqFt"
    p2 <- bearing::hist_plot(df, title_lab, x_lab, specific_variable)
    p <- c(p, p2 = list(p2))
  }

  if('PPBR' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PPBR'), msg = 'PPBR column needs to be spelt as follow: PPBR')
    specific_variable <- 'PPBR'
    title_lab <- "Histogram of Sold Price/Bedroom"
    x_lab <- "Reported Sold Price/Bedroom"
    p3 <- bearing::hist_plot(df, title_lab, x_lab, specific_variable)
    p <- c(p, p3 = list(p3))
  }

  if('PPUnit' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')
    specific_variable <- 'PPUnit'
    title_lab <- "Histogram of Sold Price/Unit"
    x_lab <- "Reported Sold Price/Unit"
    p4 <- bearing::hist_plot(df, title_lab, x_lab, specific_variable)
    p <- c(p,p4 = list(p4))
  }

  options(scipen=999)

  multiplot(plotlist = p, cols=columns)
}


#' Create a boxplot of different sales metrics
#'
#' @param df Data frame
#' @param variable variable of interest: PriceSold, PPSF, PPBR or PPUnit. default = All.
#' @param ylimits limits of y axis
#'
#' @return returns a boxplot
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' price_boxplot(Apartments3, variable = 'PPSF')
price_boxplot <- function(df, variable = c('All','PriceSold','PPSF','PPBR','PPUnit'), columns = 1){

  p <- list()

  if('PriceSold' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PriceSold'), msg = 'PriceSold column needs to be spelt as follow: PriceSold')
    specific_variable <- 'PriceSold'
    title_lab <- "Boxplot of Sold Price"
    y_lab <- "Reported Sold Price"
    p1 <- bearing::box_plot(df, title_lab, y_lab, specific_variable)
    p <- c(p, p1 = list(p1))
  }

  if('PPSF' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PPSF'), msg = 'PPSF column needs to be spelt as follow: PPSF')
    specific_variable <- 'PPSF'
    title_lab <- "Boxplot of Sold Price Per Sq Ft"
    y_lab <- "Reported Sold Price/SqFt"
    p2 <- bearing::box_plot(df, title_lab, y_lab, specific_variable)
    p <- c(p, p2 = list(p2))
  }

  if('PPBR' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PPBR'), msg = 'PPBR column needs to be spelt as follow: PPBR')
    specific_variable <- 'PPBR'
    title_lab <- "Boxplot of Sold Price/Bedroom"
    y_lab <- "Reported Sold Price/Bedroom"
    p3 <- bearing::box_plot(df, title_lab, y_lab, specific_variable)
    p <- c(p, p3 = list(p3))
  }

  if('PPUnit' %in% variable | 'All' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')
    specific_variable <- 'PPUnit'
    title_lab <- "Boxplot of Sold Price/Unit"
    y_lab <- "Reported Sold Price/Unit"
    p4 <- bearing::box_plot(df, title_lab, y_lab, specific_variable)
    p <- c(p, p4 = list(p4))
  }

  options(scipen=999)

  bearing::multiplot(plotlist = p, cols=columns)
}


#' Title
#'
#' @param df data frame
#' @param ylimits limits of y axis
#'
#' @return returns a scatter plot of reported sale price per unit vs reported sale data,
#' including a fitted line of best fit.
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' sales_time_scatter(Apartments4)
sales_time_scatter <- function(df, ylimits = c(0, 350000)){
  assertthat::assert_that(assertthat::has_name(df, 'DateSold'), msg = 'DateSold column needs to be spelt as follow: DateSold')
  assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')

  ggplot2::ggplot(df, ggplot2::aes(DateSold, PPUnit)) +
    ggplot2::geom_point(shape = 16) +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::ylim(ylimits) +
    ggplot2::labs(title = "Market Conditions", x = "Reported Sale Date", y = "Reported Sale Price Per Unit", caption = "Valuemetrics.info SGDS2")
}


#' Title
#'
#' @param df data frame
#' @param ylimits limits of y axis
#'
#' @return returns a scatter plot of reported sale price per unit vs reported average unit size,
#' including a fitted line of best fit.
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' price_avgUnitSF_scatter(NPApts)
price_avgUnitSF_scatter <- function(df, ylimits = c(0,350000)){
  assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')
  assertthat::assert_that(assertthat::has_name(df, 'AvgUnitSF'), msg = 'Average SF of unit column needs to be spelt as follow: AvgUnitSF')

  ggplot2::ggplot(df, ggplot2::aes(AvgUnitSF, PPUnit)) +
    ggplot2::geom_point(shape = 16) +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::ylim(ylimits) +
    ggplot2::labs(title = "Unit Size", x = "Reported Average Unit Size", y = "Reported Sale Price Per Unit", caption = "Valuemetrics.info SGDS2")
}



#' Average and Median Sales Price by Month
#'
#' @param df Data frame with DateSold and PriceSold variables
#'
#' @return returns plot of Average and Median Sales Price by Month
#' @export
#'
#' @examples sales_by_month(Apartments2)
sales_by_month <- function(df){
  assertthat::assert_that(assertthat::has_name(df, 'PriceSold'), msg = 'PriceSold column needs to be spelt as follow: PriceSold')
  assertthat::assert_that(assertthat::has_name(df, 'DateSold'), msg = 'DateSold column needs to be spelt as follow: DateSold')

  # remove NAs from DateSold and PriceSold if not already
  # find month and year columns
  # find average and median sales prices
  df <- df %>%
    dplyr::filter(!is.na(DateSold) | !is.na(PriceSold)) %>%
    dplyr::mutate(Year = lubridate::year(DateSold),
                  Month = lubridate::month(DateSold)) %>%
    dplyr::group_by(Year,
                    Month) %>%
    dplyr::summarise(AvgPriceSold = mean(PriceSold),
                     MedianPriceSold = median(PriceSold)) %>%
    tidyr::pivot_longer(!c(Year,Month),
                        names_to = 'Metric',
                        values_to = 'PriceSold') %>%
    dplyr::mutate(MonthYear = zoo::as.yearmon(paste(Year, Month), "%Y %m"))

  ggplot2::ggplot(df, aes(x = MonthYear, y = PriceSold, color = Metric, group = Metric)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::labs(title = "Average and Median Sales Price by Month", x = "Date", y = "Sales Price")

}


#' Plot subject property with other properties from same cluster
#'
#' @param df data frame from recombine_data_knn function
#' @param subject_ap subject property APN
#' @param subj_cluster subject property cluster
#' @param ...
#'
#' @return returns leaflet plot, with subject property and other properties from same cluster
#' @export
#'
#' @examples
plot_clusters <- function(df, subject_ap = subject_apn, subj_cluster = NULL,...){
  if(is.null(subj_cluster) & !is.null(subject_ap)){
    subject_apn <- subject_ap
    subj_cluster <- bearing::get_subj_cluster(df, subject_apn)
  }

  bearing::leaflet_plot(df %>% dplyr::filter(m == subj_cluster),
                        subject_lng,
                        subject_lat)
}


#' Plots subject property with k nearest neighbours from cluster
#'
#' @param df data frame from recombine_data_knn function
#'
#' @return returns leaflet plot, with subject property and k nearest neighbours
#' @export
#'
#' @examples
plot_knn <- function(df){
  sales_nn <- get_nn(df)

  bearing::leaflet_plot(sales_nn)
}


#' Faceted histogram
#'
#' @param df data frame
#' @param variable variable to examine across the different clusters
#'
#' @return returns histograms of the named variable for all the different clusters
#' @export
#'
#' @examples
facet_hist <- function(df, variable){
  ggplot2::ggplot(df, ggplot2::aes_string(x = variable)) +
    ggplot2::geom_histogram() +
    ggplot2::facet_grid(m ~ .)
}

#' Facet boxplot
#'
#' @param df data frame
#' @param variable variable to examine across the different clusters
#'
#' @return returns boxplots of the named variable for all the different clusters
#' @export
#'
#' @examples
facet_boxplot <- function(df, variable){
  ggplot2::ggplot(data = df) +
    ggplot2::geom_boxplot(mapping = ggplot2::aes_string(y=variable)) +
    ggplot2::guides(fill=FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(m ~ .)
}
