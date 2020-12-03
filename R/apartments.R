#' Calculating total number of bedrooms
#'
#' @param df Data frame containing info on number of bedrooms
#'
#' @return returns a data frame with additional column of NoBR
#' @export
#'
#' @examples
no_br <- function(df){
  assertthat::assert_that(assertthat::has_name(df, 'no_1br'), msg = '1BR column needs to be spelt as follow: no_1br')
  assertthat::assert_that(assertthat::has_name(df, 'no_2br'), msg = '2BR column needs to be spelt as follow: no_2br')
  assertthat::assert_that(assertthat::has_name(df, 'no_3br'), msg = '3BR column needs to be spelt as follow: no_3br')

  df %>%
    dplyr::mutate(no_br = (no_1br + (2*no_2br) + (3*no_3br)))
}


#' Creates price per metrics based off of price sold
#'
#' @param df Data frame with Price Sold information
#'
#' @return returns a data frame with additional columns: PPSF, PPUnit and PPBR
#' @export
#'
#' @examples
price_per <- function(df){
  assertthat::assert_that(assertthat::has_name(df, 'sales_price'), msg = 'PriceSold column needs to be spelt as follow: sales_price')
  assertthat::assert_that(assertthat::has_name(df, 'gba'), msg = 'SqFt column needs to be spelt as follow: gba')
  assertthat::assert_that(assertthat::has_name(df, 'no_units'), msg = 'NoUnits column needs to be spelt as follow: no_units')
  assertthat::assert_that(assertthat::has_name(df, 'no_br'), msg = 'NoBR column needs to be spelt as follow: no_br')

  df %>%
    dplyr::mutate(ppsf = sales_price / gba  ) %>%
    dplyr::mutate(ppunit = sales_price / no_units) %>%
    dplyr::mutate(ppbr = sales_price / no_br)
}


#' Function to calculate adjusted price per unit
#'
#' @param df data frame with Date Sold and PPUnit
#' @param from_date date to calculate adjustment factor from
#' @param value_date date of valuation
#'
#' @return returns a data frame with additional column: AdjPPUnit
#' @export
#'
#' @examples
adj_ppunit <- function(df, from_date, value_date){
  assertthat::assert_that(assertthat::has_name(df, 'date_sold'), msg = 'DateSold column needs to be spelt as follow: date_sold')
  assertthat::assert_that(assertthat::has_name(df, 'ppunit'), msg = 'PPUnit column needs to be spelt as follow: ppunit')

  df <- df %>%
    dplyr::filter(date_sold >= from_date)

  MktCond <- stats::lm(ppunit ~ date_sold, data = df)
  MktCondAdj <- stats::coef(MktCond)[[2]]

  df %>%
    dplyr::mutate(chg_in_days = lubridate::ymd(value_date) - lubridate::ymd(date_sold),
                  adj_ppunit = as.numeric(((MktCondAdj * chg_in_days * ppunit) + ppunit),units = "days"))

}

#' Calculates average unit square foot
#'
#' @param df data frame with number of units (NoUnits) and square foot (SqFt)
#'
#' @return returns a data frame with additional column: AvgUnitSF
#' @export
#'
#' @examples
avg_unitsf <- function(df){
  assertthat::assert_that(assertthat::has_name(df, 'gba'), msg = 'SqFt column needs to be spelt as follow: gba')
  assertthat::assert_that(assertthat::has_name(df, 'no_units'), msg = 'NoUnits column needs to be spelt as follow: no_units')

  df %>%
    dplyr::mutate(avg_unit_sf = gba / no_units)
}


#' Function to adjust price by unit size
#'
#' @param df data frame containing AvgUnitSF, PPUnit, AdjPPUnit (from adj_ppunit function)
#' @param SubjAvgUnitSF the average square ft of the subject property
#'
#' @return returns a data frame with additional column: IndPPUnit
#' @export
#'
#' @examples
adj_ppsize <- function(df, SubjAvgUnitSF){
  assertthat::assert_that(assertthat::has_name(df, 'avg_unit_sf'), msg = 'AvgUnitSF column needs to be spelt as follow: avg_unit_sf')
  assertthat::assert_that(assertthat::has_name(df, 'ppunit'), msg = 'PPUnit column needs to be spelt as follow: ppunit')
  assertthat::assert_that(assertthat::has_name(df, 'adj_ppunit'), msg = 'AdjPPUnit column needs to be spelt as follow: adj_ppunit')

  UnitSize <- stats::lm(ppunit ~ avg_unit_sf, data = df)
  SizeAdj <- stats::coef(UnitSize)[[2]]

  df %>%
    dplyr::mutate(chg_in_unit_sf = subj_avg_unit_sf - avg_unit_sf,
                  IndPPUnit = (SizeAdj* chg_in_unit_sf) + adj_ppunit)
}




