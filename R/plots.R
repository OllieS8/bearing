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
  assertthat::assert_that(assertthat::has_name(df, 'latitude'), msg = 'latitude column needs to be spelt as follow: latitude')
  assertthat::assert_that(assertthat::has_name(df, 'longitude'), msg = 'longitude column needs to be spelt as follow: longitude')

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
    leaflet::addMarkers(lng = ~longitude, lat = ~latitude, popup = paste("Address", df$address, "<br>",
                                                                        "Sale Price: $", df$sales_price, "<br>"))
}

#' Create a histogram of different sales metrics
#'
#' @param df Data frame
#' @param columns number of columns for plots - default = 1
#' @param variable variable of interest: sales_price, ppsf, ppbr or ppunit.
#'
#' @return returns a histogram
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' sales_price_histogram(Apartments3, variable = 'sales_price', bin_width = 100000, xlimits = c(0,7500000))
sales_price_histogram <- function(df, variable = c("all", "sales_price","ppsf","ppbr","ppunit"), columns = 1){

  p <- list()

  if('sales_price' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'sales_price'), msg = 'sales_price column needs to be spelt as follow: sales_price')
    specific_variable <- 'sales_price'
    title_lab <- "Histogram of Sold Price"
    x_lab <- "Reported Sold Price"
    p1 <- bearing::hist_plot(df, title_lab, x_lab, specific_variable)
    p <- c(p, p1 = list(p1))
  }

  if('ppsf' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'ppsf'), msg = 'ppsf column needs to be spelt as follow: ppsf')
    specific_variable <- 'ppsf'
    title_lab <- "Histogram of Sold Price Per Sq Ft"
    x_lab <- "Reported Sold Price/SqFt"
    p2 <- bearing::hist_plot(df, title_lab, x_lab, specific_variable)
    p <- c(p, p2 = list(p2))
  }

  if('ppbr' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'ppbr'), msg = 'ppbr column needs to be spelt as follow: ppbr')
    specific_variable <- 'ppbr'
    title_lab <- "Histogram of Sold Price/Bedroom"
    x_lab <- "Reported Sold Price/Bedroom"
    p3 <- bearing::hist_plot(df, title_lab, x_lab, specific_variable)
    p <- c(p, p3 = list(p3))
  }

  if('ppunit' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'ppunit'), msg = 'ppunit column needs to be spelt as follow: ppunit')
    specific_variable <- 'ppunit'
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
#' @param variable variable of interest: sales_price, ppsf, ppbr or ppunit. default = all.
#' @param ylimits limits of y axis
#'
#' @return returns a boxplot
#' @export
#'
#' @examples Modified from g2- Case Study.Rmd:
#' price_boxplot(Apartments3, variable = 'ppsf')
price_boxplot <- function(df, variable = c('all','sales_price','ppsf','ppbr','ppunit'), columns = 1){

  p <- list()

  if('sales_price' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'sales_price'), msg = 'sales_price column needs to be spelt as follow: sales_price')
    specific_variable <- 'sales_price'
    title_lab <- "Boxplot of Sold Price"
    y_lab <- "Reported Sold Price"
    p1 <- bearing::box_plot(df, title_lab, y_lab, specific_variable)
    p <- c(p, p1 = list(p1))
  }

  if('ppsf' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'ppsf'), msg = 'ppsf column needs to be spelt as follow: ppsf')
    specific_variable <- 'ppsf'
    title_lab <- "Boxplot of Sold Price Per Sq Ft"
    y_lab <- "Reported Sold Price/SqFt"
    p2 <- bearing::box_plot(df, title_lab, y_lab, specific_variable)
    p <- c(p, p2 = list(p2))
  }

  if('ppbr' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'ppbr'), msg = 'ppbr column needs to be spelt as follow: ppbr')
    specific_variable <- 'ppbr'
    title_lab <- "Boxplot of Sold Price/Bedroom"
    y_lab <- "Reported Sold Price/Bedroom"
    p3 <- bearing::box_plot(df, title_lab, y_lab, specific_variable)
    p <- c(p, p3 = list(p3))
  }

  if('ppunit' %in% variable | 'all' %in% variable){
    assertthat::assert_that(assertthat::has_name(df, 'ppunit'), msg = 'ppunit column needs to be spelt as follow: ppunit')
    specific_variable <- 'ppunit'
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
#' @param sales_var sales variable to use for plot. Default = sales_price
#' @param group_var variable to group on. Default = subject_m
#' @param df data frame
#'
#' @return returns a scatter plot of a sales price variable vs reported sale date,
#' including a fitted line of best fit.
#' @export
#'
#' @examples sales_time_scatter(sales_with_knn, sales_var = 'sales_price')
sales_time_scatter <- function(df, sales_var = 'sales_price', group_var = 'subject_cluster'){
  assertthat::assert_that(assertthat::has_name(df, 'sales_date'), msg = 'sales_date column needs to be spelt as follow: sales_date')

  ggplot2::ggplot(df, ggplot2::aes_string(x = 'sales_date', y = sales_var, color = group_var)) +
    ggplot2::geom_point(shape = 16) +
    ggplot2::geom_point(aes(alpha=''),
                        data=(df %>%
                                filter(pid == subject_pid)),
                        color = 'red',
                        shape = 24) +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    scale_alpha_manual(values = 1) +
    ggplot2::labs(title = "Market Conditions",
                  x = "Reported Sale Date",
                  y = sales_var,
                  alpha = 'Subject Property')
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
  assertthat::assert_that(assertthat::has_name(df, 'ppunit'), msg = 'ppunit column needs to be spelt as follow: ppunit')
  assertthat::assert_that(assertthat::has_name(df, 'avg_unit_sf'), msg = 'Average SF of unit column needs to be spelt as follow: avg_unit_sf')

  ggplot2::ggplot(df, ggplot2::aes(AvgUnitSF, ppunit)) +
    ggplot2::geom_point(shape = 16) +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::ylim(ylimits) +
    ggplot2::labs(title = "Unit Size", x = "Reported Average Unit Size", y = "Reported Sale Price Per Unit", caption = "Valuemetrics.info SGDS2")
}



#' Average and Median Sales Price by Month
#'
#' @param df Data frame with sales_date and sales_price variables
#'
#' @return returns plot of Average and Median Sales Price by Month
#' @export
#'
#' @examples sales_by_month(Apartments2)
sales_by_month <- function(df){
  assertthat::assert_that(assertthat::has_name(df, 'sales_price'), msg = 'sales_price column needs to be spelt as follow: sales_price')
  assertthat::assert_that(assertthat::has_name(df, 'sales_date'), msg = 'sales_date column needs to be spelt as follow: sales_date')

  # remove NAs from sales_date and sales_price if not already
  # find month and year columns
  # find average and median sales prices
  df <- df %>%
    dplyr::filter(!is.na(sales_date) | !is.na(sales_price)) %>%
    dplyr::mutate(year = lubridate::year(sales_date),
                  month = lubridate::month(sales_date)) %>%
    dplyr::group_by(year,
                    month) %>%
    dplyr::summarise(avg_sales_price = mean(sales_price),
                     median_sales_price = median(sales_price)) %>%
    tidyr::pivot_longer(!c(year,month),
                        names_to = 'metric',
                        values_to = 'sales_price') %>%
    dplyr::mutate(month_year = zoo::as.yearmon(paste(year, month), "%Y %m"))

  ggplot2::ggplot(df, aes(x = month_year, y = sales_price, color = metric, group = metric)) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::geom_smooth(method = lm, se = FALSE) +
    ggplot2::labs(title = "Average and Median Sales Price by Month", x = "Date", y = "Sales Price")

}


#' Plot subject property with other properties from same cluster
#'
#' @param df data frame from recombine_data_knn function
#' @param subject_ap subject property id
#' @param subj_cluster subject property cluster
#' @param ...
#'
#' @return returns leaflet plot, with subject property and other properties from same cluster
#' @export
#'
#' @examples
plot_clusters <- function(df, subject_ap = subject_pid, subj_cluster = NULL){
  if(is.null(subj_cluster) & !is.null(subject_ap)){
    subject_pid <- subject_ap
    subj_cluster <- bearing::get_subj_cluster(df, subject_pid)
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
    ggplot2::facet_grid(m ~ .,
                        labeller = label_both)
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
    ggplot2::facet_grid(m ~ .,
                        labeller = label_both)
}
