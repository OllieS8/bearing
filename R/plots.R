#' Create a leaflet plot of the subject and comparable properties
#'
#' @param df Data frame with comparable properties
#' @param subject_lng subject properties longitude
#' @param subject_lat subject properties latitude
#'
#' @return returns a leaflet plot with subject and comparable properties
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' leaflet_plot(Apartments2[1:260,], subject_lng = -117.141637, subject_lat = 32.756933)
leaflet_plot <- function(df, subject_lng, subject_lat){
  assertthat::assert_that(assertthat::has_name(df, 'Latitude'), msg = 'Latitude column needs to be spelt as follow: Latitude')
  assertthat::assert_that(assertthat::has_name(df, 'Longitude'), msg = 'Longitude column needs to be spelt as follow: Longitude')

  leaflet::leaflet(data = df) %>%
    leaflet::addTiles() %>%
    leaflet::addCircleMarkers(layerId = "subj", lng = subject_lng, lat = subject_lat, popup = "Subject Property", radius = 10, color = "red",
                     weight = 5, opacity = 1, fill = TRUE, fillColor = "red",
                     fillOpacity = 1, dashArray = NULL,  popupOptions = NULL, label = NULL, labelOptions = NULL, options = leaflet::pathOptions(), clusterOptions = NULL, clusterId = NULL) %>%
    leaflet::addMarkers(lng = ~Longitude, lat = ~Latitude, popup = ~Address)
}


#' Create a histogram of different sales metrics
#'
#' @param df Data frame of
#' @param variable variable of interest: PriceSold, PPSF, PPBR or PPUnit
#' @param bin_width width of histogram bins
#' @param xlimits x axis limits
#'
#' @return returns a histogram
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' sales_price_histogram(Apartments3, variable = 'PriceSold', bin_width = 100000, xlimits = c(0,7500000))
sales_price_histogram <- function(df, variable = c("PriceSold","PPSF","PPBR","PPUnit"), bin_width = 100000, xlimits = c(0,7500000)){

  variable <- match.arg(variable)

  if(variable == 'PriceSold'){
    assertthat::assert_that(assertthat::has_name(df, 'PriceSold'), msg = 'PriceSold column needs to be spelt as follow: PriceSold')
    title_lab <- "Histogram of Sold Price"
    x_lab <- "Reported Sold Price"
  }

  if(variable == 'PPSF'){
    assertthat::assert_that(assertthat::has_name(df, 'PPSF'), msg = 'PPSF column needs to be spelt as follow: PPSF')
    title_lab <- "Histogram of Sold Price Per Sq Ft"
    x_lab <- "Reported Sold Price/SqFt"
  }

  if(variable == 'PPBR'){
    assertthat::assert_that(assertthat::has_name(df, 'PPBR'), msg = 'PPBR column needs to be spelt as follow: PPBR')
    title_lab <- "Histogram of Sold Price/Bedroom"
    x_lab <- "Reported Sold Price/Bedroom"
  }

  if(variable == 'PPUnit'){
    assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')
    title_lab <- "Histogram of Sold Price/Unit"
    x_lab <- "Reported Sold Price/Unit"
  }

  options(scipen=999)

  ggplot2::ggplot(data = df, ggplot2::aes_string(variable)) +
    ggplot2::geom_histogram(colour="black", fill="blue", binwidth = bin_width) +
    ggplot2::xlim(xlimits) +
    ggplot2::labs(title = title_lab, x = x_lab, y = "Count", caption = "Valuemetrics.info SGDS2")
}


#' Create a boxplot of different sales metrics
#'
#' @param df Data frame
#' @param variable variable of interest: PriceSold, PPSF, PPBR or PPUnit
#' @param ylimits limits of y axis
#'
#' @return returns a boxplot
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' price_boxplot(Apartments3, variable = 'PPSF', ylimits = c(0,750))
price_boxplot <- function(df, variable = c('PriceSold','PPSF','PPBR','PPUnit'), ylimits = c(0,7500000)){

  variable <- match.arg(variable)

  if(variable == 'PriceSold'){
    assertthat::assert_that(assertthat::has_name(df, 'PriceSold'), msg = 'PriceSold column needs to be spelt as follow: PriceSold')
    title_lab <- "Boxplot of Sold Price"
    y_lab <- "Reported Sold Price"
  }

  if(variable == 'PPSF'){
    assertthat::assert_that(assertthat::has_name(df, 'PPSF'), msg = 'PPSF column needs to be spelt as follow: PPSF')
    title_lab <- "Boxplot of Sold Price Per Sq Ft"
    y_lab <- "Reported Sold Price/SqFt"
  }

  if(variable == 'PPBR'){
    assertthat::assert_that(assertthat::has_name(df, 'PPBR'), msg = 'PPBR column needs to be spelt as follow: PPBR')
    title_lab <- "Boxplot of Sold Price/Bedroom"
    y_lab <- "Reported Sold Price/Bedroom"
  }

  if(variable == 'PPUnit'){
    assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')
    title_lab <- "Boxplot of Sold Price/Unit"
    y_lab <- "Reported Sold Price/Unit"
  }

  options(scipen=999)

  ggplot2::ggplot(data = df) +
    ggplot2::geom_boxplot(mapping = ggplot2::aes_string(y=variable)) +
    ggplot2::guides(fill=FALSE) +
    ggplot2::ylim(ylimits) +
    ggplot2::labs(title = title_lab, x = "", y = y_lab, caption = "Valuemetrics.info SGDS2") +
    ggplot2::coord_flip()
}


#' Title
#'
#' @param df
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
#' @param df
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
