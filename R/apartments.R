#' Calculating total number of bedrooms
#'
#' @param df Data frame containing info on number of bedrooms
#'
#' @return returns a data frame with additional column of NoBR
#' @export
#'
#' @examples
no_br <- function(df){
  assertthat::assert_that(assertthat::has_name(df, '1BR'), msg = '1BR column needs to be spelt as follow: 1BR')
  assertthat::assert_that(assertthat::has_name(df, '2BR'), msg = '2BR column needs to be spelt as follow: 2BR')
  assertthat::assert_that(assertthat::has_name(df, '3BR'), msg = '3BR column needs to be spelt as follow: 3BR')

  df %>%
    dplyr::mutate(NoBR = (`1BR` + (2*`2BR`) + (3*`3BR`)))
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
  assertthat::assert_that(assertthat::has_name(df, 'PriceSold'), msg = 'PriceSold column needs to be spelt as follow: PriceSold')
  assertthat::assert_that(assertthat::has_name(df, 'SqFt'), msg = 'SqFt column needs to be spelt as follow: SqFt')
  assertthat::assert_that(assertthat::has_name(df, 'NoUnits'), msg = 'NoUnits column needs to be spelt as follow: NoUnits')
  assertthat::assert_that(assertthat::has_name(df, 'NoBR'), msg = 'NoBR column needs to be spelt as follow: NoBR')

  df %>%
    dplyr::mutate(PPSF = PriceSold / SqFt  ) %>%
    dplyr::mutate(PPUnit = PriceSold / NoUnits) %>%
    dplyr::mutate(PPBR = PriceSold / NoBR)
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
  assertthat::assert_that(assertthat::has_name(df, 'DateSold'), msg = 'DateSold column needs to be spelt as follow: DateSold')
  assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')

  df <- df %>%
    dplyr::filter(DateSold >= from_date)

  MktCond <- stats::lm(PPUnit ~ DateSold, data = df)
  MktCondAdj <- stats::coef(MktCond)[[2]]

  df %>%
    dplyr::mutate(ChgInDays = lubridate::ymd(value_date) - lubridate::ymd(DateSold),
                  AdjPPUnit = as.numeric(((MktCondAdj * ChgInDays * PPUnit) + PPUnit),units = "days"))

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
  assertthat::assert_that(assertthat::has_name(df, 'SqFt'), msg = 'SqFt column needs to be spelt as follow: SqFt')
  assertthat::assert_that(assertthat::has_name(df, 'NoUnits'), msg = 'NoUnits column needs to be spelt as follow: NoUnits')

  df %>%
    dplyr::mutate(AvgUnitSF = SqFt / NoUnits)
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
  assertthat::assert_that(assertthat::has_name(df, 'AvgUnitSF'), msg = 'AvgUnitSF column needs to be spelt as follow: AvgUnitSF')
  assertthat::assert_that(assertthat::has_name(df, 'PPUnit'), msg = 'PPUnit column needs to be spelt as follow: PPUnit')
  assertthat::assert_that(assertthat::has_name(df, 'AdjPPUnit'), msg = 'AdjPPUnit column needs to be spelt as follow: AdjPPUnit')

  UnitSize <- stats::lm(PPUnit ~ AvgUnitSF, data = df)
  SizeAdj <- stats::coef(UnitSize)[[2]]

  df %>%
    dplyr::mutate(ChgInUnitSF = SubjAvgUnitSF - AvgUnitSF,
                  IndPPUnit = (SizeAdj* ChgInUnitSF) + AdjPPUnit)
}




